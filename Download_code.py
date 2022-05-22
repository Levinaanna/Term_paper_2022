#Чтение больших файлов Json на примере базы данных "Подвиг народа"

import pandas as pd
import numpy as np
import ast
import json
import os

dir_name = "/Users/annalevina/Downloads/split_chelovek_nagrazhdenie"

lines_processed = []
line = " "
cnt = 0
nsplit = 1
with open('/Users/annalevina/Downloads/chelovek_nagrazhdenie.json', 'r') as f:
    while True:
        line = f.readline()
        if line:
            lines_processed.append(json.loads(line))
            cnt += 1
            if cnt % 100000 == 0:
                print(cnt)
            if cnt % 1000000 == 0:
                df = pd.DataFrame(lines_processed)
                df.to_csv("%s/chelovek_nagrazhdenie_split_%s.csv" % (dir_name, nsplit), sep = "~")
                del df
                lines_processed = []
                nsplit += 1
                print("csv is formed, lines_processed was reset")
        else:
            break
            
df = pd.DataFrame(lines_processed)
df.to_csv("%s/chelovek_nagrazhdenie_split_%s.csv" % (dir_name, nsplit), sep = "~")

files = os.listdir(path=dir_name)

df_total = pd.DataFrame()
for file in files:
    print(file)
    df = pd.read_csv(os.path.join(dir_name, file), delimiter = "~", low_memory=False, lineterminator='\n')
    df = df[["id", "rank", "mesto_priziva", "naimenovanie_nagradi", "s_kakogo_goda_v_ka",
             "data_dokumenta", "data_podviga", "id_card", "link"]]
    df_total = pd.concat([df_total, df])
    
df_total.to_csv(os.path.join(dir_name, "chelovek_nagrazhdenie.csv"), sep = "~")    
