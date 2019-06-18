from csv import DictReader


class CSVRepositoryBase:

    def __init__(self, file: str):
        self._file: str = file
        self.header = None

    def _detect_header(self):
        with open(self._file) as csv_file:
            reader = DictReader(csv_file)
            self.header = reader.fieldnames
