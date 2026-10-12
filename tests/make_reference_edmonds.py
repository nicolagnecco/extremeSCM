"""Reference costs for tests/test_edmonds.R, computed with networkx's
Chu-Liu/Edmonds (the implementation used by the QTree package) and with the
QTree package's own spanTree().

Usage (from the project root, after make_reference_qtree.py):
    git clone https://github.com/princengoc/qtree /tmp/qtree
    python3 tests/make_reference_edmonds.py /tmp/qtree/qtree

Writes tests/reference/edmonds-*.csv. Only optimal *costs* are compared in R:
when several trees have the same cost, R and networkx may return different ones.
"""
import json
import sys

import networkx as nx
import numpy as np

sys.path.insert(0, sys.argv[1])
from utils import getSupport, spanTree  # noqa: E402

REF = "tests/reference"


def nx_min_arborescence_cost(W):
    """Minimum-cost arborescence over all roots; W[i, j] = cost of i -> j."""
    d = W.shape[0]
    G = nx.DiGraph()
    G.add_nodes_from(range(d))
    for i in range(d):
        for j in range(d):
            if i != j:
                G.add_edge(i, j, weight=W[i, j])
    return nx.minimum_spanning_arborescence(G).size(weight="weight")


# 1. Random cost matrices up to 12 nodes, continuous and heavily tied costs
rng = np.random.default_rng(2026)
mats, costs = [], []
for trial in range(200):
    d = int(rng.integers(2, 13))
    W = rng.random((d, d)) if trial % 2 else rng.integers(0, 4, (d, d)).astype(float)
    np.fill_diagonal(W, np.nan)
    mats.append(W)
    costs.append(nx_min_arborescence_cost(W))

with open(f"{REF}/edmonds-random.json", "w") as f:
    json.dump({"matrices": [np.where(np.isnan(W), None, W).tolist() for W in mats],
               "costs": costs}, f)

# 2. Real QTree score matrices: the package's unrestricted root-directed tree
#    (spanTree, as in QTree.fit) and its total score
qtree = []
for dataset in ("dataset-1", "dataset-6"):
    for q in (0.7, 0.8, 0.9):
        S = np.loadtxt(f"{REF}/{dataset}-scores-q{q}.csv", delimiter=",")
        T = getSupport(spanTree(S, isMin=True)).astype(float)
        qtree.append({"dataset": dataset, "q": q,
                      "cost": float(np.nansum(S[T == 1])),
                      "tree": T.astype(int).tolist()})
with open(f"{REF}/edmonds-qtree.json", "w") as f:
    json.dump(qtree, f, indent=1)

print(f"{len(costs)} random matrices, {len(qtree)} QTree score matrices written")
