# This script is used to process webpages copied from https://www.cyclismo.org/tutorial/R/index.html site
# Which I found by looking into material from Penn State University
import os

def process_file(input_file, output_file):
    with open(input_file, 'r', encoding='utf-8') as file:
        lines = file.readlines()

    with open(output_file, 'w') as file:
        for i in range(len(lines)):
            if lines[i].startswith('>'):
                if i + 1 < len(lines) and lines[i + 1].startswith('[1]'):
                    file.write("```{r}\n")  # Replace '>' with ```{r}
                else:
                    file.write(lines[i][1:])  # Remove the '>' character
            elif not lines[i].startswith('[1]'):
                file.write(lines[i])

# Example usage
print("Current working directory:", os.getcwd())

# Get the directory of the current script
script_dir = os.path.dirname(os.path.abspath(__file__))
# Change the current working directory to the script directory
os.chdir(script_dir)

# Run the main script
process_file('input.txt', 'output.txt')

