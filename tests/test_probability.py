import pytest

from probability import Probability
from settings import TRUE, FALSE, UNKNOWN


@pytest.mark.parametrize("data,key,expected", (
        ([TRUE, FALSE], TRUE, 0.5),
        ([TRUE, FALSE, TRUE, TRUE], FALSE, 1/4),
        ([TRUE, TRUE], TRUE, 1),
        ([TRUE, TRUE], FALSE, 0),
        ([], TRUE, 0),
))
def test_should_calculate_probability(data, key, expected):
    
    assert Probability().calculate_for_key(data, key) == expected


@pytest.mark.parametrize("data,key,expected", (
        ([{1: TRUE}, {1: FALSE}], TRUE, 0.5),
        ([{1: TRUE, 2: FALSE}, {1: FALSE, 2: TRUE}], FALSE, 0.25),
        ([{1: TRUE, 2: UNKNOWN}, {1: FALSE, 2: 5}], FALSE, 0),
        ([{}, {}], FALSE, 0),
))
def test_should_calculate_probability_for_many_samples(data, key, expected):
    probability_service = Probability()
    results = probability_service.calculate_for_many_samples(data, key)

    assert results == expected


def test_changing_sample_value_will_change_total_probability():
    data = [
        {1: TRUE, 2: FALSE, 3: TRUE}, {1: FALSE, 2: FALSE, 3: FALSE}
    ]
    key = FALSE

    probability_service = Probability()
    probability_service.calculate_for_many_samples(data, key)
    results = probability_service.total

    assert results == 0.25

    probability_service.total_probability[1] = 1

    assert probability_service.total == 0.5
