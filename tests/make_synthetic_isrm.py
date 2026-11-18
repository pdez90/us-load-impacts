# A small NetCDF-3 file laid out like isrm_v1.2.1.ncf, zipped. Synthetic values.
import sys, zipfile, numpy as np
from scipy.io import netcdf_file
raw, fmt = sys.argv[1], sys.argv[2]
rng = np.random.default_rng(7)
nx, ny, d = 24, 16, 2e5; n = nx * ny; model_layers = 7
ii, jj = np.meshgrid(np.arange(nx), np.arange(ny)); ii, jj = ii.ravel(), jj.ravel()
W = -2.4e6 + ii * d; S = -1.6e6 + jj * d; E = W + d; N = S + d
cx, cy = (W + E) / 2, (S + N) / 2
tops = np.array([0, 58, 130, 253, 391, 560, 786, 1049.0])
path = raw + "/isrm_v1.2.1.ncf"
if fmt == "cdf5":
    import netCDF4
    f = netCDF4.Dataset(path, "w", format="NETCDF3_64BIT_DATA"); cd = f.createDimension
    cv = lambda nm, t, dims: f.createVariable(nm, t, dims)
else:
    f = netcdf_file(path, "w", version=2); cd = f.createDimension
    cv = lambda nm, t, dims: f.createVariable(nm, t, dims)
for nm, k in (("layer", 3), ("source", n), ("receptor", n), ("allcells", n * model_layers + n // 4), ("layers", 3)): cd(nm, k)
v = cv("layers", "i4", ("layers",)); v[:] = np.array([0, 3, 6], dtype="i4")
dist = np.hypot(cx[:, None] - cx[None, :], cy[:, None] - cy[None, :]) / 1e6
for k, nm in enumerate(["PrimaryPM25", "SOA", "pNH4", "pNO3", "pSO4"]):
    v = cv(nm, "f4", ("layer", "source", "receptor"))
    for l in range(3):
        v[l] = (1e-9 * (k + 1) / (1 + l) * np.exp(-dist * (3 - l)) * rng.uniform(0.5, 1.5, (n, n))).astype("f4")
def allc(a): return np.concatenate([np.tile(a, model_layers), a[:n // 4]])   # plus a coarser top layer
pop = rng.gamma(0.5, 1.5e6, n)
for nm, a in (("TotalPop", pop), ("MortalityRate", rng.uniform(600, 1000, n)), ("WhiteNoLat", pop * 0.6), ("Black", pop * rng.uniform(0, .3, n)),
              ("Latino", pop * 0.15), ("Asian", pop * 0.05), ("Native", pop * 0.01), ("WindSpeed", rng.uniform(1, 9, n)),
              ("N", N), ("S", S), ("E", E), ("W", W)):
    v = cv(nm, "f8", ("allcells",)); v[:] = allc(a)
top = lambda a, x: np.concatenate([np.repeat(a, n), np.full(n // 4, x)])
v = cv("LayerHeight", "f8", ("allcells",)); v[:] = top(tops[:-1], 1049.0)
v = cv("Dz", "f8", ("allcells",)); v[:] = top(np.diff(tops), 400.0)
v = cv("Layer", "f8", ("allcells",)); v[:] = top(np.arange(model_layers, dtype="f8"), 7.0)
f.close()
with zipfile.ZipFile(raw + "/isrm_v1.2.1.zip", "w", zipfile.ZIP_DEFLATED) as z:
    z.write(path, "isrm_v1.2.1.ncf"); z.writestr("marginal_values.csv", "a,b\n1,2\n")
print("synthetic ISRM written (%s), %d cells" % (fmt, n))
