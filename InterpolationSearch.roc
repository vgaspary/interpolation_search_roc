InterpolationSearch :: [].{

	a.Comparable :  where [a.is_lt : a, a -> Bool, a.is_lte : a, a -> Bool]

	interpolation_search : (a, a, a -> F32), List(a), a -> U64 where [a.Comparable]
	interpolation_search = |interpolation_factor, lst, target| match lst {
		[] => 0
		[only] if only < target => 1
		[_] => 0
		[first, ..] if target <= first => 0
		[.., last] if last < target => lst.len()
		[first, .., last] => {
			f = interpolation_factor(first, target, last)
			mid_idx = ((lst.len() - 1).to_f32() * f).to_u64_wrap()
			match lst.get(mid_idx) {
				Ok(mid) => if !(mid < target) {
					interpolation_search(interpolation_factor, lst.take_first(mid_idx), target)
				} else {
					interpolation_search(interpolation_factor, lst.drop_first(mid_idx + 1), target) + mid_idx + 1
				}
				_ => crash "`interpolation_factor` must return an F32 value in [0.0, 1.0] range."
			}
		}
	}

	# Tests

	## Dec
	interpolation_factor_dec = |a, b, c| (b - a) / (c - a)
	interpolation_search_dec = |lst, target| interpolation_search(interpolation_factor_dec, lst, target)

	expect interpolation_search_dec([1.0.F32, 2.0, 3.0], 0.0) == 0
	expect interpolation_search_dec([1.0.F32, 2.0, 3.0], 1.0) == 0
	expect interpolation_search_dec([1.0.F32, 2.0, 3.0], 2.0) == 1
	expect interpolation_search_dec([1.0.F32, 2.0, 3.0], 3.0) == 2
	expect interpolation_search_dec([1.0.F32, 2.0, 3.0], 4.0) == 3

	## I32
	interpolation_factor_i32 = |a, b, c| (b - a) / (c - a)
	interpolation_search_i32 = |lst, target| interpolation_search(interpolation_factor_i32, lst, target)

	expect interpolation_search_i32([1, 2, 3, 3, 3, 4, 5, 6], 0) == 0
	expect interpolation_search_i32([1, 2, 3, 3, 3, 4, 5, 6], 3) == 2

	## Custom type
	Item := { val : F32 }.{
		is_lt : Item, Item -> Bool
		is_lt = |a, b| a.val < b.val

		is_lte : Item, Item -> Bool
		is_lte = |a, b| a.val <= b.val
	}
	interpolation_factor_item : Item, Item, Item -> F32
	interpolation_factor_item = |a, b, c| (b.val - a.val) / (c.val - a.val)
	interpolation_search_item = |lst, target| interpolation_search(interpolation_factor_item, lst, target)

	test_list : List(Item)
	test_list = [{ val: 1.0 }, { val: 2.0 }, { val: 3.0 }]
	expect interpolation_search_item(test_list, Item.{ val: 2.0 }) == 1
}
