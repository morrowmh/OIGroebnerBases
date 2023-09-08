#!/usr/bin/env python3

pkg_name = "OIGroebnerBases"

with open("pkg_src/" + pkg_name + ".m2", "r") as file:
    lines = file.readlines()

# Process load commands
i = 0
while True:
    if lines[i].startswith("load "):
        with open("pkg_src/" + lines[i].split("\"")[1]) as module_file:
            module_lines = module_file.readlines()
        
        length = len(module_lines)
        
        # Insert lines from module
        for j in range(length):
            lines.insert(i + j, module_lines[j])
        
        i = i + length # Jump over inserted lines
        lines[i] = "\n" # Remove load command
    else: i = i + 1

    if i >= len(lines): break
    if lines[i].startswith("end"): break

# Output final package
with open(pkg_name + ".m2", "w") as output:
    output.writelines(lines)