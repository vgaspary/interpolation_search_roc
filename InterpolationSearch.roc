InterpolationSearch :: [].{

	interpolation_search : List(a), a -> U64 where [a.is_lte : a, a -> Bool, a.is_gt : a, a -> Bool, a.interpolation_factor : a, a, a -> F32]
	interpolation_search = |lst, target| match lst {
		[] => 0
		[first, ..] if target <= first => 0
		[.., last] if target > last => lst.len()
		[first, .., last] => {
			f = first.interpolation_factor(target, last)
			mid_idx = ((lst.len() - 1).to_f32() * f).to_u64_wrap()
			match lst.get(mid_idx) {
				Ok(mid) => if target <= mid {
					interpolation_search(lst.take_first(mid_idx), target)
				} else {
					interpolation_search(lst.drop_first(mid_idx + 1), target) + mid_idx + 1
				}
				_ => crash "mid index was out of range"
			}
		}
		_ => crash "What case is not covered? I don't understand"
	}

	# Tests

	## Custom type
	Item := { val : F32 }.{
		is_lte = |a, b| a.val <= b.val
		is_gt = |a, b| a.val > b.val
		interpolation_factor = |first, target, last| (target.val - first.val) / (last.val - first.val)
	}

	test_list : List(Item)
	test_list = [{ val: 1.0 }, { val: 2.0 }, { val: 3.0 }]
	expect interpolation_search(test_list, { val: 2.0 }) == 1

	## Floats
	# interpolation_factor : F32, F32, F32 -> F32
	# interpolation_factor = |a, b, c| (b - a) / (c - a)
	# expect interpolation_search([1.0.F32, 2.0, 3.0], 2.0) == 1

}
