import pymol
from pymol import cmd

# 启动 PyMOL 引擎
pymol.finish_launching()


def hex_to_rgb(hex_color):
    hex_color = hex_color.lstrip("#")
    return tuple(int(hex_color[i : i + 2], 16) / 255.0 for i in (0, 2, 4))


pdbs = {
    "6HX3": "#fce4ec",  # 浅粉色
    "1QCB": "#fff3e0",  # 浅橙色
    "1TOT": "#fffde7",  # 浅黄色
    "3RG6": "#f3e5f5",  # 浅紫色
}

for pdb_id, bg_hex in pdbs.items():
    cmd.reinitialize()
    cmd.load(f"./data/hw2/{pdb_id}.pdb")

    # 设置背景色 (修正：先注册自定义 RGB 颜色，再应用)
    rgb_color = hex_to_rgb(bg_hex)
    cmd.set_color(f"custom_bg_{pdb_id}", rgb_color)
    cmd.bg_color(f"custom_bg_{pdb_id}")

    # 隐藏默认显示，开启卡通和表面模式
    cmd.hide("everything")
    cmd.show("cartoon")
    cmd.show("surface")

    # 4. 按链着色 (Spectrum by chain)
    cmd.spectrum("chain", palette="rainbow")

    # 5. 设置表面颜色和透明度 (修正：使用正确的 transparency setting)
    cmd.set("surface_color", "white", pdb_id)  # 将该对象的表面设为白色
    cmd.set(
        "transparency", 0.4, pdb_id
    )  # 将该对象的整体透明度设为 0.4 (0=不透明, 1=全透明)

    # 6. 调整视角，让蛋白质居中并稍微放大
    cmd.orient()
    cmd.zoom("visible", 2)

    # 7. 导出高分辨率图片 (ray=1 开启光线追踪，渲染更精美)
    cmd.png(
        f"./results/hw2/{pdb_id}_render.png", width=1200, height=1200, dpi=300, ray=1
    )
    print(f"成功导出: ./results/hw2/{pdb_id}_render.png")

print("所有蛋白质渲染完成！")
cmd.quit()
