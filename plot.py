#!/usr/bin/env python
import matplotlib.pyplot as plt
import pandas as pd

data = pd.read_table("benchmark.csv")

algorithms = data["algorithm"].unique()
patternlengths = sorted(data["pattern"].unique())
texts = data["text"].unique()

cmap = plt.get_cmap("gist_rainbow")
colors = {algorithm: cmap(i / len(algorithms))
          for i, algorithm in enumerate(algorithms)}

groups = data.groupby("text")

fig, axes = plt.subplots(nrows=len(texts), ncols=1, figsize=(8, 16))

for text, axis in zip(texts, axes):
    axis.set_ylabel(text)
    textdata = data[data["text"] == text]
    for algorithm in algorithms:
        algodata = textdata[textdata["algorithm"] == algorithm]
        plot = axis.loglog(algodata["pattern"],
                           algodata["search_time"],
                           color=colors[algorithm],
                           label=algorithm)
    if text == texts[0]:
        axis.legend(loc='lower left',
                    bbox_to_anchor=(0, 1.2, 1, 1),
                    ncol=3,
                    fontsize='small',
                    mode='expand')
    axis.set_yticklabels([])
    axis.set_xticks([], minor=True)
    axis.set_xticks(patternlengths)
    axis.set_xticklabels(patternlengths, fontsize='xx-small')
axis.set_xlabel("patternlength")

plt.tight_layout(rect=(0, 0, 1, 0.94))
plt.savefig("benchmark.png")
