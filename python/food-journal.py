import os, glob, subprocess

directory = os.listdir("C:\\Users/kesha/Documents/notes/") if os.name == "nt" else glob.glob("~/Documents/notes/*")
files = [file for file in directory if "_food_telemetry" in file]

dates = {}
for file_name in files:
    date = file_name[:8]
    if date in dates:
        dates[date].append(file_name)
    else:
        dates[date] = [file_name]

table = {}        
for date, files in dates.items():
    table[date]: [str] = []
    for file in files:
        [table[date].append(string.strip()) for string in str(subprocess.check_output(f"grep \"|\" ~/Documents/notes/{file}")).split("|")
         if string not in ["b'", "\\r\\n", "\\r\\n'", '']]

for date, calories in table.items():
    print(date, sum(map(lambda x: int(x) if x else 0, calories[1::2])))
