from abc import ABCMeta, abstractmethod
from csv import DictReader, DictWriter
from typing import Iterable, Dict

from repositories.csv_base import CSVRepositoryBase


class AnswersRepository(metaclass=ABCMeta):

    @abstractmethod
    def get_all(self) -> Iterable[Dict]:
        raise NotImplementedError("Not implemented")

    @abstractmethod
    def save(self, answers: Dict):
        raise NotImplementedError("Not implemented")


class CSVAnswersRepository(AnswersRepository, CSVRepositoryBase):

    def get_all(self) -> Iterable[Dict]:
        with open(self._file) as csv_file:
            reader = DictReader(csv_file)

            for row in reader:
                yield row

    def save(self, answers: Dict):
        with open(self._file, 'a') as csv_file:
            if self.header is None:
                self._detect_header()

            writer = DictWriter(csv_file, self.header)
            writer.writerow(answers)
