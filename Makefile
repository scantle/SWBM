# -------------------------------------------------------------------------
# Compiler and flags
FC = gfortran
BUILD_DIR = build
MOD_DIR = $(BUILD_DIR)/mod
BIN_DIR = bin
# Common flags:
#  -O2      => optimize
#  -cpp     => enable C preprocessor (#includes, etc.)
#  -Wall    => enable common warnings
#  -J./mod  => put .mod files in ./mod
#  -I./mod  => tell compiler to look for .mod in ./mod
FFLAGS = -O2 -cpp -Wall -J$(MOD_DIR) -I$(MOD_DIR) \
         -I./Libraries/flibs/src/strings -I./Libraries/flibs \
         -static -static-libgfortran -static-libgcc

# -------------------------------------------------------------------------
# Build targets
PROGRAM    = $(BIN_DIR)/SWBM
FLIBS_LIB  = $(BUILD_DIR)/libflibs.a

# -------------------------------------------------------------------------
# Source files

# FLIBS sources
FLIBS_SRC = \
  Libraries/flibs/src/strings/m_vstring.f90 \
  Libraries/flibs/src/strings/m_vstringlist.f90

# SWBM sources
SWBM_SRC = \
  src/Lists.f90 \
  src/FileIO.f90 \
  src/global.f90 \
  src/ErrorHandler.f90 \
  src/options.f90 \
  src/time.f90 \
  src/define_fields.f90 \
  src/irrigation.f90 \
  src/water_mover.f90 \
  src/read_main_input.f90 \
  src/irr_ditch.f90 \
  src/outputmodule.f90 \
  src/SWBM.f90

# -------------------------------------------------------------------------
# Convert source filenames to object filenames inside build/
FLIBS_OBJ = $(patsubst %.f90, $(BUILD_DIR)/%.o, $(FLIBS_SRC))
SWBM_OBJ  = $(patsubst %.f90, $(BUILD_DIR)/%.o, $(SWBM_SRC))

# -------------------------------------------------------------------------
# "make all" will compile the library and then the main program
.PHONY: all
all: $(PROGRAM)

# -------------------------------------------------------------------------
# Compile FLIBS first
$(FLIBS_LIB): $(FLIBS_OBJ)
	mkdir -p $(BUILD_DIR)
	ar rcs $@ $^

# Pattern rule to compile FLIBS modules
$(BUILD_DIR)/%.o: %.f90
	mkdir -p $(dir $@)  # Ensure the full path exists!
	$(FC) $(FFLAGS) -c $< -o $@

# -------------------------------------------------------------------------
# Build the main SWBM program, ensuring it finds FLIBS modules
$(PROGRAM): $(FLIBS_LIB) $(SWBM_OBJ)
	mkdir -p $(BIN_DIR)
	$(FC) $(FFLAGS) -o $@ $(SWBM_OBJ) -L$(BUILD_DIR) -lflibs

# -------------------------------------------------------------------------
# Clean up
.PHONY: clean
clean:
	rm -rf $(BUILD_DIR) $(BIN_DIR)