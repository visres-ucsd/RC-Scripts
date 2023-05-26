# Reading Center Scripts

This is a repository for all of the reading center scripts Nicole has contributed to. Below is a quick description of each folder and the purpose each script is supposed to serve. If there are any questions regarding any of the scripts in this repository, please email Nicole (nibrye@health.ucsd.edu).

<hr>

### Table of Contents

1. [`HFA3_60_4_Defect_Tool`](####`HFA3_60_4_Defect_Tool`) 
2. [`REDCAP_Merge`](####`REDCAP_Merge`)
3. [`RF_VFxmlParse`](####`RF_VFxmlParse`)
4. [`pdf-ocr`](####`pdf-ocr`)
5. [`pdf-pdfquery`](####`pdf-pdfquery`)
6. [`vf-dicom`](####`vf-dicom`)

<hr>

#### `HFA3_60_4_Defect_Tool`

This folder contains the scripts needed to interact with and use the second version (v2.0.0.0) of the HFA3 60-4 Defect Tool (`HFA_get_60_4_defect_depth_dicom_raw_opv.exe`) on large batches of raw dicom files. The tool / MATLAB executable itself is not enclosed in this folder. If the tool itself is needed, please email Nicole to see about acquiring the tool and all of the necessary dependcies + instructions.

**There are two scripts enclosed in this folder:**

- `run_tool.bat`
  - This is a batch file used to run the HFA3 60-4 Defect Tool on all of the DICOM files in a specified input directory (the resulting output csv files will be placed in a folder called `output` within the same directory). In order to specify the directories of interest, you will need to right click `run_tool.bat` in your file manager, and then click `Edit`. Once the text editor has opened for you to make edits, change the content inside of the quotes `" "` directly following `set tool_fp=` to the file path of where the tool (`HFA_get_60_4_defect_depth_dicom_raw_opv.exe`) is located on your machine. In addition, change the content inside of the qutoes `" "` directly following `set dicom_fp=` to the file path of a directory containing the DICOM files you would like to be processed. **Make sure you save your changes to the file before proceeding.**


  - Once the file paths have been specified and `run_tool.bat` has been saved, simply close the text editor and double click on `run_tool.bat` to run it on all of the DICOM files in the specified input directory. If everything has been set up correctly, a messaged should print for every 60-4 DICOM file that is processed, and a folder called `output` should be created and populated with the outputs.


  - **NOTE:** Try to limit file names to those containing alphanumeric characters and underscores. Spaces in file names tend to cause troubles for the HFA3 60-4 Defect Tool.


- `Add_defect_depths_V1.ipynb`
  - This is a Python notebook used to populate the output csvs in `output` (see instructions for `run_tool.bat`) with Defect Depth measurements. This script should only be used after running `run_tool.bat`. 

  - In order to use this script, you will need to specify three file paths: the file path pointing to the location of the `output` folder, the file path pointing to the location of the csv file with defect depth measurements, and the file path pointing to the location where you want the updated csv to be outputted to. Upon opening the script in Jupyter Notebook, you will see placeholders to but all of these file paths with brief instructions of where to put each one. After updating the file paths, run all cells in the notebook to add defect depth measurements.

<br>

#### `REDCAP_Merge`

This folder contains three versions of a script (`redcap_eyechart_merge`) that I did not create, but made edits to. While I've enclosed all of the scripts that contain my edits in this folder, the relevant script is the most recent version: `redcap_eyechart_merge_v2.2.ipynb`. This includes the edits for handling a glaucoma medications that contains duplicate rows, and for classifying `"OTHER (Specify)"` medications as glaucoma. 

#### `RF_VFxmlParse`

#### `pdf-ocr`

#### `pdf-pdfquery`

#### `vf-dicom`
  

