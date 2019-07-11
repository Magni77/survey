import logging

from exceptions import UnexpectedQuestionAnswer
from settings import Answers


logger = logging.getLogger(__name__)


class Question:

    def __init__(self, slug: str, content: str, expected_answer: str):
        self.slug = slug
        self.content: str = content
        self.expected_answer: str = expected_answer
        self.user_answer: Answers = Answers.UNKNOWN

    def __str__(self):
        return f"Question({self.slug})"

    def __repr__(self):
        return f"Question({self.slug})"

    @property
    def expected_answer(self) -> Answers:
        return self._expected_answer

    @expected_answer.setter
    def expected_answer(self, new_value: str):
        map_ = {
            Answers.TRUE.value: Answers.TRUE,
            Answers.FALSE.value: Answers.FALSE,
            Answers.UNKNOWN.value: Answers.UNKNOWN
        }

        try:
            self._expected_answer = map_[new_value]
        except KeyError:
            logger.exception('Unexpected question answer')
            raise UnexpectedQuestionAnswer('Answers should be one of [T, F, U]')
