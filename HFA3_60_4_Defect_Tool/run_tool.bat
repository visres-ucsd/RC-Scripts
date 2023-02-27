@echo off

Rem CHANGE BELOW TO THE FILE PATH CONTAINING THE TOOL
set tool_fp="\Users\nicolebrye\Downloads\Hfa3Tool60_4_v2.0.0.0"

Rem CHANGE BELOW TO THE FILE PATH CONTAINING THE DICOMS 
set dicom_fp="\Users\nicolebrye\Downloads\AF_DCM_Matlab20210818-Z7VjKdO6X4"


cd \

for /r %%i in (%dicom_fp%\*.dcm) do (

cd %tool_fp%
HFA_get_60_4_defect_depth_dicom_raw_opv.exe -f %%i %dicom_fp%\output
cd \)


@pause