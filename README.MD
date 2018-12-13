SETUP

Install sbt package (using apt-get).
httpie is used for testing

RUNNING APPLICATION AND UNIT TESTS

'sbt run', 'sbt test' from terminal

USAGE

Index a book:
http --verbose --json POST http://localhost:9000/v1/book title='Annihilation' author='Jeff Vandermeer' pages[0]='The first page' pages[1]='The second page'

Search for a book page with content 'first':
 http --verbose GET http://localhost:9000/v1/book?query=first