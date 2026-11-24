import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# 1. 读取数据
df = pd.read_csv("./data/hw1/points.txt", sep="\s+")

# 2. 设置绘图风格
plt.figure(figsize=(8, 8), dpi=300)
ax = plt.gca()

# 3. 根据 target type 分类绘图 (模仿原图的红叉和蓝圈)
# hard 和 very 分数较低，用红色 x；easy 和 triv 分数较高，用蓝色 o
mask_hard_very = df["target type"].isin(["hard", "very"])
mask_easy_triv = df["target type"].isin(["easy", "triv"])

# 绘制散点 (设置透明度 alpha)
ax.scatter(
    df.loc[mask_hard_very, "Xtmscore"],
    df.loc[mask_hard_very, "Ytmscore"],
    marker="x",
    color="#d9534f",
    s=50,
    alpha=0.7,
    label="Hard/Very",
)
ax.scatter(
    df.loc[mask_easy_triv, "Xtmscore"],
    df.loc[mask_easy_triv, "Ytmscore"],
    marker="o",
    facecolors="none",
    edgecolors="#428bca",
    s=50,
    alpha=0.7,
    label="Easy/Triv",
)

# 4. 绘制辅助线
# 对角线 y = x
x_line = np.linspace(0, 1, 100)
ax.plot(x_line, x_line, color="black", linewidth=1.5, linestyle="-")

# 虚线阈值 (x=0.5, y=0.5)
ax.axhline(0.5, color="black", linewidth=1.5, linestyle="--")
ax.axvline(0.5, color="black", linewidth=1.5, linestyle="--")

# 5. 设置坐标轴和标签
ax.set_xlim(0, 1)
ax.set_ylim(0, 1)
ax.set_xlabel("TM-score of I-TASSER", fontsize=14, fontweight="bold")
ax.set_ylabel("TM-score of C-I-TASSER", fontsize=14, fontweight="bold")
ax.tick_params(axis="both", which="major", labelsize=12)

# 6. 导出图片 (导出为 PDF 或 SVG 在 AI 中更清晰，但 PNG 300dpi 也足够)
plt.tight_layout()
plt.savefig("scatter_plot.png", dpi=300)
# plt.savefig('scatter_plot.pdf') # 推荐导出 PDF，在 AI 里可以无损放大
plt.show()
