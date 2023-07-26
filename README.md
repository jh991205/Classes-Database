# Class Roster Database for CS Students

This repository contains a Class Roster Database designed to assist Computer Science (CS) students in selecting classes that fulfill graduation requirements and provide information about those classes. The data for the classes is pulled from the Cornell Class Roster API and stored locally, allowing for manual updates following the instructions in the `install.txt` file.

## Installation

Before proceeding with the installation, ensure that you have the following dependencies installed on your system:

- OCAML
- OPAM
- SSL, cohttp, and core libraries

Follow these steps to set up the database:

1. Download and extract the files from this repository.
2. Open a terminal and navigate to the extracted directory.
3. Run `make build` in the terminal to build the necessary components.
4. Run `make play` in the terminal to start using the Class Roster Database.

## How to Use

1. The application will prompt you to input a major. To do so, type `major "some major"`. You will be provided with a list of available majors. If you wish to quit, simply type `quit`. If you want information about double majoring, use the command `doublemajor "major1" "major2"`.
2. Next, the application will ask you to input the college associated with the chosen major. Use the command `college "some college name"`. A list of college names will be provided for your convenience.
3. If you input an unsupported major or college name, the app will prompt you to provide supported answers.
4. The app will then display some courses relevant to the selected major(s) and ask whether you want to see more detailed courses within specific categories. You can respond with yes or no.
5. Finally, the app will inquire if there is a specific course you would like to know more about, including its schedule and instructors. To obtain this information, use the command `course "some course name"`. A list of course names will be given. If you are not interested in further details, simply type `notinterested`. To quit the application, use the command `quit`.

Feel free to play the game from the start with any major and college combination you desire.

Note: To update the data manually, follow the instructions provided in the `install.txt` file.

Enjoy using the Class Roster Database to plan your academic journey and make informed course selections!
