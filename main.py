from probability import Probability
from repositories.answers_repository import CSVAnswersRepository
from repositories.questions_repository import CSVQuestionsRepository
from survey import Survey

if __name__ == '__main__':
    questions_repository = CSVQuestionsRepository('data/questions.csv')
    answers_repository = CSVAnswersRepository('data/first_survey.csv')
    probability_service = Probability()

    s = Survey(questions_repository, answers_repository, probability_service)
    s.start()
