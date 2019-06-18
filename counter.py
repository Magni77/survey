from typing import Collection, Dict, Iterable
from collections import defaultdict


class KeyCounter:
    """
    Simple counter created for counting elements in iterable of dicts
    """

    def __new__(cls, data: Iterable[Dict]) -> Dict:
        cls.counted = defaultdict(
            lambda: defaultdict(
                lambda: 0
            )
        )

        for row in data:
            for field_name, value in row.items():
                cls.counted[field_name][value] += 1

        return cls.counted
