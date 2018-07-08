# tables

Statistical and mathematical tables, typeset with LaTeX

Now includes percentiles of the Gaussian, student, chi-square, and
Fisher distributions; and the distribution function of the Gaussian.

To make them, type 'make' on the commandline.  To do it by hand,
source all the R files (this will create txt files and PDF files),
then pdflatex the tex files to create the tables.  You will need to
latex files twice to get the logo in the right position.

R files of the form `foo_diagram.R` create the little pictures of the
density functions such as `student_diagram.pdf`; R files of the form
`foo_number_maker.R` create text files with properly rounded and
typeset table entries, such as `student_table_values.txt`.

All the finished tables are PDF format and have filenames of the
form `table_foo.pdf`.






