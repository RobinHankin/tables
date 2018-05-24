all:
	R CMD BATCH gaussian_diagram.R
	R CMD BATCH gaussian_number_maker.R
	pdflatex table_gaussian_distribution_function
	R CMD BATCH chisq_diagram.R
	R CMD BATCH chisq_number_maker.R
	pdflatex  table_chisq

clean:
	rm -f  *.aux *.log *.out *.Rout *.txt gaussian_diagram.pdf table_gaussian_distribution_function.pdf

