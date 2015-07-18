
test('T2120', normal, compile_and_run, [''])
test('largeArray', normal, compile_and_run, [''])
test('array001', [
    extra_clean(['array001.data']),
    expect_broken_for(10659, ['hpc', 'optasm', 'threaded2', 'dyn', 'optllvm']),
  ],
  compile_and_run, [''])

test('T9220', normal, ghci_script, ['T9220.script'])
