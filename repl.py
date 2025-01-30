import os
import readline

def path_completer(text, state):
    """Return the next possible completion for 'text'.

    This function is used by the readline module to provide tab completion.
    """
    if not text:
        completions = [f"{x}/" if os.path.isdir(x) else x for x in os.listdir('.')]
    else:
        completions = [f"{x}/" if os.path.isdir(x) else x for x in os.listdir('.') if x.startswith(text)]

    try:
        return completions[state]
    except IndexError:
        return None

def main():
    # Set up readline to use our custom completer
    readline.set_completer(path_completer)
    readline.parse_and_bind("tab: complete")

    print("Welcome to the Path Completer REPL. Type 'exit' to quit.")
    while True:
        try:
            user_input = input(">>> ")
            if user_input.strip().lower() == 'exit':
                print("Exiting...")
                break
            elif user_input:
                print(f"You entered: {user_input}")
        except EOFError:
            print("\nExiting...")
            break

if __name__ == "__main__":
    main()
