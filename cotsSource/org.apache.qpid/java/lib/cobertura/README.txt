Download the cobertura binary from the following location:

http://cobertura.sourceforge.net/download.html


Unpack it into the cobertura (this) directory with tar --strip-path 1 -xf.
This should leave you with cobertura.jar in qpid/java/lib/cobertura.

Run "ant cover-test coverage-report" to generate coverage report.

