
test('T2120', normal, compile_and_run, [''])
test('largeArray', normal, compile_and_run, [''])
test('array001',    extra_clean(['array001.data']), compile_and_run, [''])

test('T9220', expect_broken(9220), ghci_script, ['T9220.script'])
