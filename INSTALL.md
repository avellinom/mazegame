# Install Guide
Run the following commands in terminal to install/use the maze generator:

1. Install the graphics module. You may be prompted to enter your password:

    `$ opam install graphics`

2. Install other required packages as listed from the previous command.
These may include pressing "y" and 

    `$ opam user-setup install`
***

If you want to see some demos while our generator is developing, run the following command:

1. Build the executables:

    `$ dune build`

2. Run demos:

    `$ dune exec examples/ocean_sunset.exe`

    `$ dune exec examples/random_cubism.exe`
