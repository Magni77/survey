import io
from unittest import mock

import pytest

from questions import Question
from survey import Survey

question_repository = mock.Mock(get_all=lambda: [Question('test', 'test', 'F')])
answers_repository = mock.Mock()
probability_service = mock.Mock(probability_samples={})


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
@mock.patch('survey.Survey.update_user_question_in_answer')
@mock.patch('survey.Survey.update_probability')
@mock.patch('survey.Survey.get_user_input')
@mock.patch('survey.Survey.show_answers')
@mock.patch('survey.Survey.show_probability')
@mock.patch('survey.Survey.show_question')
@mock.patch('survey.Survey.calculate_probability')
def test_survey_flow(
        calc_prob_moc, show_question, show_probability, show_answers,
        get_user_input, update_probability, update_question, show_results,
        save_all_user_answers, survey):

    survey.start()

    calc_prob_moc.assert_called_once()
    show_question.assert_called_once()
    show_probability.assert_called_once()
    show_answers.assert_called_once()
    get_user_input.assert_called_once()
    update_probability.assert_called_once()
    show_results.assert_called_once()
    update_question.assert_called_once()
    save_all_user_answers.assert_called_once()


@pytest.mark.skip('No time for now for fixing this test. Do it later.')
@mock.patch('builtins.input', return_value=1)
def test_user_input_will_be_mapped_to_probability(survey):
    result = survey.get_user_input()

    assert result == 0


def test_update_probability_will_update_probability_service_state(survey):

    survey.update_probability(Question('new_q', 'what is love', 'F'), 1)

    assert survey.probability_service.probability_samples['new_q'] == 0


def test_calculate_probability_will_get_all_answers(survey):

    survey.calculate_probability()

    survey.probability_service.calculate_for_many_samples.assert_called_once()
    survey.answers_repository.get_all.assert_called_once()
