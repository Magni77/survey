import operator
from collections import Counter, defaultdict
from functools import reduce
from typing import Collection, Any, Dict, Iterable

from exceptions import InCorrectProbabilityException


def prod(iterable):
    return reduce(operator.mul, iterable, 1)


class Probability:
    """
    Service for calculation probability.
    Can be used for simple structures like lists with calculate_for_key
        ex
            data = [1, 2, 1, 2]
            probability = Probablity().calculate_for_key(data, 1)
            probability == 0.5

    Or for Iterable[Dict]
            data = [{1: T, 2: F}, {1: F, 2: T}]
            probability = Probablity().calculate_for_many_samples(data, 1)
            probability == 0.25

            because for key=1 prob=1/2 and for key=2 prob = 1/2
            1/2 * 1/2 = 0.25

    Probability of any sample can be changed by accessing total_probability dict
    # TODO There should be validation of what user can put into dictionary to do not break code
    """

    def __init__(self):
        self.probability_samples = {}

    @property
    def total(self):
        if self.probability_samples:
            return prod(self.probability_samples.values())
        return 0

    @staticmethod
    def calculate_for_key(data: Collection, key: Any):
        try:
            counter = Counter(data)
            return counter.get(key) / sum(counter.values())
        except TypeError:
            return 0

    def calculate_for_many_samples(self, data: Iterable[Dict], key: Any):
        counter_data = self._count_elements_in_iterable_of_dicts(data)
        for sample, counted_values in counter_data.items():
            self.probability_samples[sample] = self.calculate_for_key(counted_values, key)

        return self.total

    def update_sample(self, slug: str, probability: int):
        if not 0 <= probability <= 1:
            raise InCorrectProbabilityException('Probability should have value between 0 and 1')

        self.probability_samples[slug] = probability

    @staticmethod
    def _count_elements_in_iterable_of_dicts(data: Iterable[Dict]) -> defaultdict:
        counter = defaultdict(
            lambda: defaultdict(
                lambda: 0
            )
        )

        for row in data:
            for field_name, value in row.items():
                counter[field_name][value] += 1

        return counter
