InterpolationSearch :: [].{

	a.Comparable :  where [a.is_lt : a, a -> Bool]
	interpolation_search : (a, a, a -> F32), List(a), a -> U64 where [a.Comparable]
	interpolation_search = |interpolation_factor, lst, target| match lst {
		[] => 0
		[only] if only < target => 1
		[_] => 0
		[first, ..] if !(first < target) => 0
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

	expect {
		Item := { val : F32 }.{
			is_lt = |a, b| a.val < b.val
		}
		interpolation_factor_item : Item, Item, Item -> F32
		interpolation_factor_item = |a, b, c| (b.val - a.val) / (c.val - a.val)
		interpolation_search_item = |lst, target| interpolation_search(interpolation_factor_item, lst, target)

		test_list : List(Item)
		test_list = [{ val: 1.0 }, { val: 2.0 }, { val: 3.0 }]
		interpolation_search_item(test_list, Item.{ val: 2.0 }) == 1
	}

	a.Numeric :  where [a.Comparable, a.minus : a, a -> a, a.to_f32 : a -> F32]
	interpolation_search_num : List(a), a -> U64 where [a.Numeric]
	interpolation_search_num = |lst, target| interpolation_search(|a, b, c| (b - a).to_f32() / (c - a).to_f32(), lst, target)

	expect interpolation_search_num([1, 2, 3, 3, 3, 4, 5, 6], 0) == 0
	expect interpolation_search_num([1, 2, 3, 3, 3, 4, 5, 6], 3) == 2
	expect interpolation_search_num([1, 2, 3, 3, 3, 4, 5, 6], 12) == 8
	expect interpolation_search_num([1.0, 2.0, 3.0], 0.0) == 0
	expect interpolation_search_num([1.0, 2.0, 3.0], 1.0) == 0
	expect interpolation_search_num([1.0, 2.0, 3.0], 2.0) == 1
	expect interpolation_search_num([1.0, 2.0, 3.0], 3.0) == 2
	expect interpolation_search_num([1.0, 2.0, 3.0], 4.0) == 3
	expect interpolation_search_num([1.0.F32, 2.0, 3.0], 0.0) == 0

}
