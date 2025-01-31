# SWBM
SWBM (**Soil-Water Budget Model**)

## 🏗️ Building SWBM

SWBM can be compiled using **Intel Fortran (ifort) via Visual Studio** or **gfortran using a Makefile**.

### **Building with Intel Fortran in Visual Studio**
If you are using **Intel Fortran**, simply open the **`SWBM.sln`** file in **Visual Studio** and build the solution.

### **Building with gfortran (Windows/Linux/macOS)**
If you prefer **gfortran**, you can use the included `Makefile`. This has only been tested on **Windows (MSYS2/MinGW)**

#### **Prerequisites**
- **Windows**: Install [MSYS2](https://www.msys2.org/) and gfortran (included in gcc):
  ```bash
  pacman -S mingw-w64-x86_64-gcc-fortran make
  ```
- **Linux/macOS**: Install gfortran and make via your package manager.

#### Compile Using Make
```bash
make
```

## Running SWBM
Once compiled, the executable can be run from the command line:
```bash
SWBM [main input file name]
```

As set of example files have been included in the folder example_input_files/