COBOL MERGE AND SORTED WAREHOUSE INVENTORY SALES ![Creator](https://img.shields.io/badge/Created%20By-Tyler%20Brown-blue.svg)
======================

This program helps merge different records from different warehouses and sort the data. The input data for this program is stored in PR3AF17A.TXT and PR3AF17B.TXT. The program uses this input file to generate the sales report which is outputed as PROJECT 3 REPORT.TXT. In order to generate this report the program first begins to merge the 2 seperate files(PR3AF17A.TXT and PR3AF17B.TXT) that contain sales based on warehouses in each state. This merged data is then stored in "PR4 MERGED.txt" and used in the next step in generating the report file. Next the program sorts through the data arrangeing alphabetically by State, City, and Then by customer and stores the data in "PR4 MERGED.txt". After this is done the program begins to use the data generated so far to create the report file for the user. This report file displays data on residing warehouses, customer data, customer ratings, product data, sales based on products, sales based on customer, sales from each city, sales in total for an entire state, and all sales in total for the given business period.

# File Structure PR3F17A.txt
![alt text](file%20structure/file%20structureA.PNG "PR3F17A.txt")

# File Structure PR3F17B.txt
![alt text](file%20structure/file%20structureB.PNG "PR3F17B.txt")

# Usage
This project is useful for the creation of sales report based on warehouses. This reports also helps give insight on what customers are looking for geographically and what their spending habits are for that region. There is also a customer rating system displayed in the report that shows how active a customer is with the company.

# COBOL
* This project was written using OpenCOBOL and was designed to run on none mainframe computers.
* This code is compatible with COBOL version 89 and above
* ANSI COBOL STANDARDS USED: X3.23a-1989

# Author
* [tylerbro93](https://github.com/tylerbro93/)

# License
This project is created under the [MIT License](./LICENSE)

# NOTES
* This project can be modified and used for free
* Give credit if used
