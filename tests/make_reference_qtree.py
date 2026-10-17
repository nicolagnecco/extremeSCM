"""Reference outputs for tests/test_fit_qtree.R, computed with the authors' QTree
package (https://github.com/princengoc/qtree) and, for order-restricted trees,
with networkx's Chu-Liu/Edmonds on the forward edges only (plus brute force for
small graphs). Nothing here uses the R port.

Usage (from the project root):
    git clone https://github.com/princengoc/qtree /tmp/qtree
    python3 tests/make_reference_qtree.py /tmp/qtree/qtree

Inputs: tests/reference/<dataset>-X.csv (log(x + 1) of a fixed subsample of the
declustered data; columns in causal order, upstream first).
"""
import itertools
import json
import sys

import networkx as nx
import numpy as np
import pandas as pd

sys.path.insert(0, sys.argv[1])
from QTree import QTree, quantileDispersion  # noqa: E402
from utils import getSupport, spanTree  # noqa: E402

REF = "tests/reference"
SMALL_R = 0.05
Q_GRID = (0.7, 0.8, 0.9)
N_SUB, FRAC = 50, 0.75
meta = {"small_r": SMALL_R, "q_grid": Q_GRID, "cases": []}


def order_consistent(A):
    """Every edge i -> j has i before j (columns are in causal order)."""
    return bool(np.all(np.triu(A, 1) == A))


def brute_force_tree(S):
    """Minimum-score tree in which node k picks one child among k+1, ..., d-1."""
    d = S.shape[0]
    best = min(itertools.product(*[range(k + 1, d) for k in range(d - 1)]),
               key=lambda ch: sum(S[k, c] for k, c in enumerate(ch)))
    A = np.zeros((d, d))
    A[np.arange(d - 1), best] = 1
    return A


def edmonds_restricted(S, kind):
    """Optimal sink-directed tree using only edges i -> j with i < j, via
    networkx's Edmonds algorithm (same reversal trick as the package's spanTree)."""
    d = S.shape[0]
    G = nx.DiGraph()
    G.add_nodes_from(range(d))
    for i in range(d):
        for j in range(i + 1, d):
            G.add_edge(j, i, weight=S[i, j])     # reversed edge
    T = (nx.minimum_spanning_arborescence(G) if kind == "min"
         else nx.maximum_spanning_arborescence(G))
    A = np.zeros((d, d))
    for u, v in T.edges():
        A[v, u] = 1
    return A


def save(name, A):
    np.savetxt(f"{REF}/{name}.csv", np.asarray(A, dtype=float), delimiter=",", fmt="%.17g")


for dataset in ("dataset-1", "dataset-6"):
    df = pd.read_csv(f"{REF}/{dataset}-X.csv")
    df.columns = range(df.shape[1])
    d = df.shape[1]

    # 1. Score matrices: row i = source i (score of edge i -> j)
    for q in Q_GRID:
        S = np.vstack([quantileDispersion(i, df, SMALL_R, q, None) for i in range(d)])
        save(f"{dataset}-scores-q{q}", S)

        # 2. Order-restricted tree, by Edmonds on the forward edges; for small
        #    graphs also by brute force (they must agree).
        T = edmonds_restricted(S, "min")
        if d <= 8:
            assert (T == brute_force_tree(S)).all()
        save(f"{dataset}-tree-q{q}", T)
        T_pkg = getSupport(spanTree(S, isMin=True)).astype(float)
        meta["cases"].append({"dataset": dataset, "q": q,
                              "package_tree_order_consistent": order_consistent(T_pkg)})

    # 3. Centroid and variability (QTree.fitToSamples) on fixed subsamples
    rng = np.random.default_rng(1)
    subs = [np.sort(rng.choice(len(df), int(np.floor(len(df) * FRAC)), replace=False))
            for _ in range(N_SUB)]
    model = QTree(smallR=SMALL_R, q=0.8, weights=None)
    trees = [model.fit(df.iloc[s].reset_index(drop=True)).astype(float) for s in subs]
    e_T, var_T = model.fitToSamples([df.iloc[s].reset_index(drop=True) for s in subs])
    np.savetxt(f"{REF}/{dataset}-centroid-subsamples.csv", np.vstack(subs) + 1, fmt="%d")
    np.savetxt(f"{REF}/{dataset}-centroid-trees.csv",
               np.vstack([t.reshape(1, -1) for t in trees]), delimiter=",", fmt="%d")
    save(f"{dataset}-centroid-package", e_T.astype(float))
    W = np.sum(trees, axis=0) / N_SUB
    save(f"{dataset}-centroid-restricted", edmonds_restricted(W, "max"))
    meta["cases"].append({"dataset": dataset, "centroid_q": 0.8, "var_T_package": var_T})

with open(f"{REF}/meta.json", "w") as f:
    json.dump(meta, f, indent=1)
print(json.dumps(meta, indent=1))
