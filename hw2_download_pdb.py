import urllib.request

for pid in ["6HX3", "1QCB", "1TOT", "3RG6"]:
    urllib.request.urlretrieve(
        f"https://files.rcsb.org/download/{pid}.pdb", f"./data/hw2/{pid}.pdb"
    )
    print("OK: ", pid)
