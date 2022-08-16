#######################################################################
# Master script
#######################################################################

## Clear out existing environment
rm(list = ls()) 

### GiT directories
git_mdir <- "/Users/hhadah/Documents/GiT/VoterParticipation"
tables_wd <- paste0(git_mdir,"/outputs/tables")
figures_wd <- paste0(git_mdir,"/outputs/figures")
raw_wd <- paste0(git_mdir,"/data/raw")
datasets_wd <- paste0(git_mdir,"/data/datasets")

programs <- paste0(git_mdir,"/programs")

### run do files and scripts
source(file.path(programs,"01_packages_wds.r")) # set up packages

# Send Message

textme(message = "👹 Back to work! You're not paid to run around and drink ☕ all day!")