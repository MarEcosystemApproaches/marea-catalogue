# UPDATING MAREA CATALOGUE


# this document outlines the steps for updating the marea catalogue.
# updates can be made as new variables enter the marea package,
# or over time when data in the marea package updates.
# some processes for updating are (or can be) automatic,
# but some chunks will require manual edits.

# source update helpers
source("update_procedure/02-update-helpers.R")



############### Part I: Adding new chapters #########################
# The following procedure outlines steps for creating indicator pages
# in the marea catalogue that do not currently exist.
# This is for the initialization of chapters, or for when new
# variables are added to marea.

# step 1. build indicator page --------------------------------------------
# source file with helper functions for auto-generating new pages
create_marea_page("azmp_satellite_temperature")


# step 2. manually edit page template -------------------------------------
# navigate to chapters/{indicator}.Rmd to add text, specifications,
# and detail to indicator template page. Some sections will auto-fill,
# but many will need manual edits (e.g., description, relevance to fisheries).


# step 3. add to yml ------------------------------------------------------
# once the page is complete, navigate to ./_bookdown.yml and
# add the newly generated page path ( chapters/{indicator}.Rmd)
# to the yml file so that it shows in the book.
# note that the chapters will appear in the order they are entered.
# follow section headings to add new indicators to the correct sections.


# step 4.  ----------------------------------------------------------------
# use Build -> Build Book to regenerate the book with the new pages.



############### Part II: Updating existing chapters #########################
# The following procedure outlines steps for updating existing chapters that
# have been created and manually supplemented. The purpose of this workflow
# is to update the chapters with the most up-to-date marea data (e.g., data
# over longer durations than when the chapter was created), but maintain the
# information that was added manually.


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# run update function
# use this function to auto-generate the structure of a new marea
# indicator page, which will auto-populate into the chapters/ folder.
# if the chapter already exists, the package will ask if it should be overwritten.

# NOTE:
# the function returns an error if the indicator already exists.
# that's because overwriting an existing chapter will clear any manual
# edits already made. In this situation, I recommend changing the names
# of the existing files (amo.rmd to amo_old.rmd), then running this workflow
# to create a new chapter file (amo.rmd), and copy/paste the manual additions
# from amo_old.Rmd to amo.Rmd.

# in the future, I will work on harvesting manual additions from existing
# chapters when possible.
create_marea_page(indicator = "mei")





