test('T2120', normal, compile_and_run, [''])
test('largeArray', normal, compile_and_run, [''])
test('array001', [
    extra_clean(['array001.data']),
  ],
  compile_and_run, [''])

test('T9220', normal, ghci_script, ['T9220.script'])
test('T229', [exit_code(1)], compile_and_run, [''])
