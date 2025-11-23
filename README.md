# SWBM
The SWBM (**Soil-Water Budget Model**) is a land use/crop-soil water budget model that simulates agricultural practices to estimate hydrologic fluxes at the field scale using a tipping bucket approach. The method is documented in [Foglia et al. (2013)](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/wrcr.20555) and [Tolley et al. (2019)](https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2018WR024209). SWBM is a standalone model, but is designed to write input files for MODFLOW 2000/2005/NWT, and can thus be used with those groundwater codes.

This is a modified version of ["official" SWBM](https://github.com/gustolley/SWBM) maintained by [gustolley](https://github.com/gustolley). Some of the changes include:
- Block input file specifying settings, model discretization, folder locations
- Support for daily output of SFR inflows via tabfiles (`DAILY_SW`)
- An "absolute irrigation date" by which all fields will be set to irrigate (`ABSOLUTE_IRR_DATE`)
- Irrigation Ditch package for simulating irrigation ditch diversions (via the SFR package) and seepage (via injections in the WEL package)
- The MNW2 writer was downgraded to write MODFLOW WEL files
- Water Mover package for moving water between different compartments
- Water use curtailments can be input using the `CURTAIL_FRAC` file
- Monthly corrections (multipliers) to field pET via the `ET_CORRECTION` file
- Managed Aquifer Recharge (MAR) can be input using the `MAR_DEPTH` file
- Wells are either agricultural wells (rates are predicted by model) or specified wells (rates are specfied in a file and passed to the GW model)
- Overlap with MODFLOW model is given through a fractional overlap file, where a field can supply a percent of it's recharge/excess ET to any/multiple MODFLOW cells

This version may contain features/changes specific to the [Scott Valley Integrated Hydrologic Model (SVIHM)](https://github.com/scantle/SVIHM).

## 🏗️ Building SWBM
Windows executable files are included in the [bin/](./bin/) folder, but if necessary, SWBM can be compiled using **Intel Fortran (ifort) via Visual Studio** or **gfortran using a Makefile**.

### **Building with Intel Fortran in Visual Studio**
If you are using **Intel Fortran**, simply open the **`SWBM.sln`** file in **Visual Studio** and build the solution.

### **Building with gfortran (Windows/Linux/macOS)**
If you prefer **gfortran**, you can use the included `Makefile`. This has only been tested on **Windows (MSYS2/MinGW)**

#### **Prerequisites**
- **Windows**: Install [MSYS2](https://www.msys2.org/) (or a standalone MinGW-w64 distribution) and gfortran (included in gcc):
  ```bash
  pacman -S mingw-w64-x86_64-gcc-fortran make
  ```
- **Linux/macOS**: Install gfortran and make via your package manager.

#### Compile Using Make
Once you've setup MinGW-w64 you can easily build the executable using the makefile:
```bash
make
```
This will add a build/ folder where libraries and other intermediate files will be generated. The executable itself will be generated in the bin/ folder

## Running SWBM
Once compiled, the executable can be run from the command line:
```bash
SWBM [main input file name]
```

As set of example files have been included in the directory example_input_files/.
