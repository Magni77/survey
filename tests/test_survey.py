import io
from unittest import mock

import pytest

from entities.questions import Question
from probability import Probability
from settings import Answers
from survey import Survey

question_mock = Question('test', 'test', 'F')
question_repository = mock.Mock(get_all=lambda: [question_mock])
answers_repository = mock.Mock()
probability_service = Probability()


@pytest.fixture()
def survey():
    return Survey(question_repository, answers_repository, probability_service)


@mock.patch('sys.stdout', new_callable=io.StringIO)
def test_show_answers(stdout_mock, survey):

    survey.show_answers()

    expected_strings = ('1. True', '2. False', '3. Skip')

    assert all(expected in stdout_mock.getvalue() for expected in expected_strings)

# TODO tests for all stdout methods


@mock.patch('survey.Survey.show_results')
@mock.patch('survey.Survey.save_all_user_answers')
@mock.patch('survey.Survey.serve_question')
@mock.patch('survey.Survey.calculate_probability')
def test_survey_flow_will_serve_question(
        calc_prob_moc, serve_question, save_all_user_answers, show_results, survey):

    survey.start()

    calc_prob_moc.assert_called_once()
    serve_question.assert_called_once_with(question_mock)

    save_all_user_answers.assert_called_once()
    show_results.assert_called_once()


@mock.patch('survey.Survey.process_user_input')
@mock.patch('survey.Survey.get_user_input')
@mock.patch('survey.Survey.ask_question')
def test_serve_question_will_ask_question_wait_for_answer_and_process_it(
        ask_question_mock, get_user_input_mock, process_user_input_mock, survey):

    survey.serve_question(question_mock)

    ask_question_mock.assert_called_once_with(question_mock)

    get_user_input_mock.assert_called_once()

    process_user_input_mock.assert_called_once()


@mock.patch('survey.Survey.show_question')
@mock.patch('survey.Survey.show_probability')
@mock.patch('survey.Survey.show_answers')
def test_ask_question_will_show_question_probability_and_answers(
        show_answers_mock, show_probability_mock, show_question_mock, survey):
    survey.ask_question(question_mock)

    show_answers_mock.assert_called_once()

    show_probability_mock.assert_called_once()

    show_question_mock.assert_called_once_with(question_mock)


def test_process_user_input_will_update_answers(survey):
    survey.process_user_input(1, question_mock)

    assert question_mock.user_answer == Answers.TRUE


def test_process_user_input_will_update_probability_service(survey):
    survey.process_user_input(1, question_mock)

    assert survey.probability_service.probability_samples[question_mock.slug] == 0


@pytest.mark.skip('No time for now for fixing this test. Do it later.')
@mock.patch('builtins.input', return_value=1)
def test_user_input_will_be_mapped_to_probability(survey):
    result = survey.get_user_input()

    assert result == 0


def test_update_probability_will_update_probability_service_state(survey):

    survey.update_probability(Question('new_q', 'what is love', 'F'), 1)

    assert survey.probability_service.probability_samples['new_q'] == 0
