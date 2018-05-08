all:
	R CMD BATCH gaussian_diagram.R
	pdflatex table_gaussian_distribution_function

clean:
	rm -f  *.aux *.log *.out *.Rout gaussian_diagram.pdf table_gaussian_distribution_function.pdf

