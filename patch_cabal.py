import sys


def patch(exec_name: str, file_path: str):
    new_content = []
    with open(file_path, "r") as f:
        exec_found = False
        for line in f.readlines():
            if f"executable {exec_name}" in line:
                exec_found = True
            if exec_found and "default-language" in line:
                new_content.append("    , ghc-experimental\n")
                exec_found = False
            new_content.append(line)

    with open(file_path, "w") as f:
        f.write("".join(new_content))


def main():
    exec_name = sys.argv[1]
    file_path = sys.argv[2]
    patch(exec_name, file_path)


if __name__ == "__main__":
    main()
