from abc import ABCMeta, abstractmethod
from csv import DictReader
from typing import Iterable

from entities.questions import Question
from repositories.csv_base import CSVRepositoryBase


class QuestionsRepository(metaclass=ABCMeta):

    @abstractmethod
    def get_all(self) -> Iterable[Question]:
        raise NotImplementedError("Not implemented")


class CSVQuestionsRepository(QuestionsRepository, CSVRepositoryBase):

    def get_all(self) -> Iterable[Question]:
        with open(self._file) as csv_file:
            reader = DictReader(csv_file)

            return [Question(**row) for row in reader]
