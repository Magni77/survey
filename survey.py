from typing import Iterable

from probability import Probability
from entities.questions import Question
from repositories.answers_repository import AnswersRepository
from repositories.questions_repository import QuestionsRepository

from settings import Answers


class Survey:

    def __init__(self, questions_repository: QuestionsRepository,
                 answers_repository: AnswersRepository,
                 probability_service: Probability):

        self.questions_repository: QuestionsRepository = questions_repository
        self.answers_repository: AnswersRepository = answers_repository
        self.probability_service: Probability = probability_service
        self.correct_answer = Answers.FALSE.value
        self.map_input_to_probability = {
            1: 0,
            2: 1,
            3: 0
        }
        self.map_input_to_answer = {
            1: Answers.TRUE,
            2: Answers.FALSE,
            3: Answers.UNKNOWN
        }

    def start(self):
        self.calculate_probability()

        questions = self.questions_repository.get_all()

        for question in questions:
            self.serve_question(question)

        self.save_all_user_answers(questions)

        self.show_results()

    def serve_question(self, question: Question):
        self.ask_question(question)

        user_input = self.get_user_input()

        self.process_user_input(user_input, question)

    def ask_question(self, question: Question):
        self.show_question(question)
        self.show_probability()
        self.show_answers()

    def process_user_input(self, user_input: int, question: Question):
        self.update_user_answer_in_question(question, user_input)
        self.update_probability(question, user_input)

    def show_results(self):
        if self.probability_service.total == 1:
            print('Congratulations! You can become panelist!')
        else:
            print('Unfortunately you can not become panelist..')

    def update_probability(self, question: Question, user_input: int):
        probability = self.map_input_to_probability[user_input]
        self.probability_service.update_sample(question.slug, probability)

    def update_user_answer_in_question(self, question: Question, user_input: int):
        question.user_answer = self.map_input_to_answer[user_input]

    def calculate_probability(self):
        self.probability_service.calculate_for_many_samples(
            self.answers_repository.get_all(), self.correct_answer
        )

    def save_all_user_answers(self, questions: Iterable[Question]):
        self.answers_repository.save(
            {
                question.slug: question.user_answer.value
                for question
                in questions
            }
        )

    def get_user_input(self):
        try:
            input_ = int(
                input('Press 1/2/3 \n')
            )
            if input_ not in [1, 2, 3]:
                raise ValueError
            return input_
        except ValueError:
            return self.get_user_input()

    def show_probability(self):
        print(
            f"Probability for becoming panelist: "
            f"{self.probability_service.total}"
        )

    @staticmethod
    def show_question(question: Question):
        print(question.content)

    @staticmethod
    def show_answers():
        print(
            """
            1. True
            2. False
            3. Skip
            """
        )
