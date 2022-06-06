import geopandas
import pandas as pd
from matplotlib.ticker import FuncFormatter
gdf = geopandas.read_file("data/ru-all.geo.json")
gdf.rename({"name": "geo_name"}, axis = 1, inplace = True)
gdf = gdf[~pd.isnull(gdf["geo_name"])]
map_names = pd.read_csv("data/map_names.csv", sep = ";", encoding = "utf-8")
gdf = gdf.merge(map_names, on = "geo_name", how = "left")
repressions = pd.read_csv("data/репрессии.csv", sep = ";", encoding = "utf-8")
repressions.rename({"Регион 1937-1940": "name_represii_old_region"}, axis = 1, inplace = True)
gdf = gdf.merge(repressions[["name_represii_old_region", "Арестовано в 1937 и 1938 по Мозохину на душу населения",
                             "Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения"]],
                on = "name_represii_old_region", how = "left")
gdf.set_index("name_represii_new_region", inplace = True)
gdf['Арестовано в 1937 и 1938 по Мозохину на душу населения'] = gdf['Арестовано в 1937 и 1938 по Мозохину на душу населения'].astype(float)
gdf['Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения'] = gdf['Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения'].astype(float)
fmt = lambda x, pos: '{:.1%}'.format(x)

ax = gdf.plot("Количество репрессированных в 1937 и 1938 по Мемориалу на душу населения", figsize = [20, 10],
              legend = True, cmap = 'Blues', missing_kwds= dict(color = "lightgrey"), 
              legend_kwds = {"format": FuncFormatter(fmt)})
ax.axis('off')

ax = gdf.plot("Арестовано в 1937 и 1938 по Мозохину на душу населения", figsize = [20, 10],
              legend = True, cmap = 'Blues', missing_kwds= dict(color = "lightgrey"),
              legend_kwds = {"format": FuncFormatter(fmt)})
ax.axis('off')
