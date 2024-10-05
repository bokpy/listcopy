import subprocess
import sys

def get_mbid(file_path):
    command = ['picard', '--tag-from-file', '--quiet', '--format', '{"mbid": "%mbid%"}', file_path]
    result = subprocess.run(command, capture_output=True, text=True)

    if result.returncode == 0:
        return result.stdout.strip()['mbid']
    else:
        return None

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <file_path>")
        sys.exit(1)

    file_path = sys.argv[1]
    mbid = get_mbid(file_path)

    if mbid:
        print(f"MBID: {mbid}")
    else:
        print("Could not find an MBID for the file.")
