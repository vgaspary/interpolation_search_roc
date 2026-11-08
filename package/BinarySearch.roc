BinarySearch :: [].{

	binary_search : List(a), (a -> Bool) -> U64
	binary_search = |lst, pred| match lst {
		[] => 0
		[first, ..] if pred(first) => 0
		[.., last] if !pred(last) => lst.len()
		_ => {
			mid_idx = lst.len() / 2
			match lst.get(mid_idx) {
				Ok(mid) => if pred(mid) {
					binary_search(lst.take_first(mid_idx), pred)
				} else {
					binary_search(lst.drop_first(mid_idx + 1), pred) + mid_idx + 1
				}
				_ => crash "not possible"
			}
		}
	}

	# Empty list
	expect binary_search([], |n| n >= 0) == 0

	# Single element
	expect binary_search([5], |n| n >= 0) == 0
	expect binary_search([-5], |n| n >= 0) == 1

	# Two elements
	expect binary_search([1, 2], |n| n >= 0) == 0
	expect binary_search([-1, 1], |n| n >= 0) == 1
	expect binary_search([-2, -1], |n| n >= 0) == 2

	# All elements false
	expect binary_search([-5, -4, -3], |n| n >= 0) == 3
	expect binary_search([-4, -3, -2, -1], |n| n >= 0) == 4

	# All elements true
	expect binary_search([1, 2, 3], |n| n >= 0) == 0
	expect binary_search([0, 1, 2, 3], |n| n >= 0) == 0

	# First match in left half
	expect binary_search([-5, 0, 1, 2, 3], |n| n >= 0) == 1
	expect binary_search([-5, 0, 1, 2, 3, 4], |n| n >= 0) == 1

	# First match at or around middle
	expect binary_search([-2, -1, 0, 1, 2], |n| n >= 0) == 2
	expect binary_search([-3, -2, -1, 0, 1, 2], |n| n >= 0) == 3

	# First match in right half
	expect binary_search([-5, -4, -3, -2, 0], |n| n >= 0) == 4
	expect binary_search([-5, -4, -3, -2, -1, 0], |n| n >= 0) == 5
	expect binary_search([-5, -4, -3, -2, 0, 1], |n| n >= 0) == 4
	expect binary_search([-4, -3, -2, -1, 0, 1, 2], |n| n >= 0) == 4

	# First match at the very last index
	expect binary_search([-3, -2, -1, 0], |n| n >= 0) == 3
	expect binary_search([-4, -3, -2, -1, 0], |n| n >= 0) == 4

	# Lists with duplicates
	expect binary_search([-2, -1, 0, 0, 0], |n| n >= 0) == 2
	expect binary_search([0, 0, 0, 1, 2], |n| n >= 0) == 0
	expect binary_search([-2, -1, 0, 0, 1], |n| n >= 0) == 2
	expect binary_search([-2, -2, -2, -1, -1, 0, 0, 1, 1], |n| n >= 0) == 5
	expect binary_search([0, 0, 0, 0], |n| n >= 0) == 0
	expect binary_search([-1, -1, -1, -1], |n| n >= 0) == 4

	# Different types: Strings (by byte length)
	expect binary_search(["a", "bb", "ccc", "dddd", "eeeee"], |s| s.count_utf8_bytes() >= 3) == 2

	# Different types: Booleans
	expect binary_search([False, False, True, True], |b| b) == 2
	expect binary_search([False, False, False], |b| b) == 3
	expect binary_search([True, True, True], |b| b) == 0

	# Different types: Floats
	expect binary_search([0.1, 0.2, 0.5, 0.8, 1.2], |x| x >= 0.5) == 2

	# Number threshold
	expect binary_search([10, 20, 30, 40, 50], |n| n >= 35) == 3

}
