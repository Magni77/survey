from probability import Probability
from questions import Question
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

    def start(self):
        self.calculate_probability()

        for question in self.questions_repository.get_all():

            self.show_question(question)
            self.show_probability()
            self.show_answers()

            user_answer = self.get_user_answer()

            self.update_probability(question, user_answer)

        self.show_results()
        # TODO Restart mechanism

    def show_results(self):
        if self.probability_service.total == 1:
            print('Congratulations! You can become panelist!')
        else:
            print('Unfortunately you can not become panelist..')

    def update_probability(self, question:Question, user_answer: int):
        self.probability_service.total_probability[question.slug] = user_answer

    def calculate_probability(self):
        self.probability_service.calculate_for_many_samples(
            self.answers_repository.get_all(), self.correct_answer
        )

    def get_user_answer(self):
        try:
            input_ = input('Press 1/2/3 \n')
            return self.map_input_to_probability[
                int(input_)
            ]
        except(ValueError, KeyError):
            self.get_user_answer()

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
