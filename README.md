# survey


What Is This?
-------------
This is simple CLI program that asks the users 5 questions and calculate probability of this user 
meeting the criteria for becoming a panelist, based on their answers so far and the data 
from all the other candidates.


Survey data is loaded with generators so program could handle big amount of data.
It's also easy extensible for different data sources thanks to repository pattern.

Questions are also easy editable in /data/questions.csv file

Requirements
---------------
`Python >= 3.7`


How To Use This
---------------

1. Run `python main.py`


Testing
---------------
1. Run `pip install -r requirements.txt` to install dependencies
2. Run `pytest`


TODO
---------------
- add missing tests cases
- consider moving from from dict to Answer object for survey data
- add main menu with restart mechanism (?)
- catch missing exceptions
