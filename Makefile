all:
	R CMD BATCH gaussian_diagram.R
	R CMD BATCH gaussian_number_maker1.R
	pdflatex table_gaussian_distribution_function
	pdflatex table_gaussian_distribution_function
	R CMD BATCH gaussian_number_maker2.R
	R CMD BATCH gaussian_number_maker3.R
	pdflatex table_gaussian_percentiles
	pdflatex table_gaussian_percentiles
	R CMD BATCH student_number_maker.R
	pdflatex table_student
	pdflatex table_student
	R CMD BATCH chisq_diagram.R
	R CMD BATCH chisq_number_maker.R
	pdflatex table_chisq
	pdflatex table_chisq

clean:
	rm -f  *.aux *.log *.out *.Rout *.txt chisq_diagram.pdf gaussian_diagram?.pdf table*.pdf *.tex~

