project('my_test', 'fortran', 'c')

fortran_dialect = [ '-ffree-form','-std=f2018','-fimplicit-none']
fortran_bounds = [ '-fbounds-check']

add_global_arguments(fortran_dialect, language : 'fortran')
add_global_arguments(fortran_bounds, language : 'fortran')

serialport = dependency('libserialport')

util_lib_sources = ['src/serialport_utils.c']
util_lib = static_library('serialport_utils', util_lib_sources, dependencies : [serialport])


cmain_sources = ['src/main.c']
cmain_inc_dir = include_directories('src')
executable('cmain', cmain_sources, include_directories : cmain_inc_dir, link_with: util_lib, dependencies : [serialport])

fmain_sources = [
    'src/main.f90', 
    'src/serialport_kinds.f90',
    'src/serialport_types.F90', 
    'src/serialport_utils.f90', 
    'src/serialport_info.f90',
    'src/serialport_list.f90',
    'src/serialport_dev.f90',
    'src/serialport_config.f90',
    ]
executable('fmain', fmain_sources, dependencies : [serialport], link_with : util_lib, link_language : 'fortran' )


