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

<br>

#### `RF_VFxmlParse`

This folder contains 7 versions of a script originally authored by James, which I took over. The script is designed to parse XML files, and output the information in csv format divided by test type. The script is currently updated to work for 10-2, 24-2, 24-2C, 30-2, and 60-4 test types. The script is also currently updated to work on either a single directory of XML files, or a directory containing multiple directories of XML files.

The most relevant version of the script is `RF_VFxmlParse_V.23.R`. This script contains all of the updates listed above.

In order to use the script, change the file path within `file.path("...")` to the file path pointing to where the XML files are stored.

<br>

#### `pdf-ocr`

This folder contains 4 versions of a script used to extract information from visual field PDFs using AWS optical character recognition. Since the OCR provided by AWS is not very accurate on visual field PDFs, there likely won't be much need for this script. In addition, the setup for these scripts is rather involved due to the interactions with AWS (specifically S3 and the AWS Software Development Kit for Python, `boto3`).

In order to properly set up your environment to use this script, please see the [Boto3 Documentation](https://boto3.amazonaws.com/v1/documentation/api/latest/guide/quickstart.html) for installing `boto3` and ensuring that your S3 buckets can be accessed via Jupyter. While executing this process a few other resources will be helpful (both linked in the `boto3` documentation), but here they are as well for convenience:

- [Creating IAM Users](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_create.html#id_users_create_console)
- [Managing Access Keys](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html#Using_CreateAccessKey)

These will help with giving yourself the proper user permissions, as well as retrieving your access keys to use `boto3`.

If this script is needed and there are any troubles with setup, please contact Nicole.

<br>

#### `pdf-pdfquery`

This folder contains two versions of a script used to extract information from visual field PDFs using a Python library called `pdfquery`. While this script performs the same task as the scripts enclosed in `pdf-ocr`, it is more accurate and quicker as well. This script is also updated to work on the probability plots in each visual field PDF (non-text data). 

Upon opening the script in Jupyter, adjust the input and output directories to the desired locations in order to use the script (more detailed instructions are enclosed in the script). This script is currently designed to work on one directory containing visual field PDFs, and not a directory containing other directories.

If there are any troubles importing the `pdfquery` package once the script has been run, please contact Nicole.

<br>

#### `vf-dicom`

This folder contains the scripts used to manage the dicom files from forum.

**The two most relevant scripts in this folder are as follows:**

- `extract_pdf_from_forum_dicom_v6.0.0.ipynb`
  - This is a Python notebook used to go through a directory of dicom files or a directory containing multiple directories of dicom files. This script's purpose is to convert each dicom file to a PDF, rename it based on a `PatientName_Eye_ExamDate_TestType_ExamTime` naming schema, and move it to a new folder based on the test type. The script will create the stratified test type folders if they are not created on your computer already.

  - Upon opening the script in Jupyter, adjust the input and output directories to the desired locations in order to use the script (more detailed instructions are enclosed in the script). This script is currently designed to work on one directory containing dicom files, and not a directory containing other directories.

If there are any troubles importing the `pydicom` package once the script has been run, please contact Nicole (this is usually indicated by an Error right after the code box that is labeled `Imports`).


- `extract_pdf_from_pdfs_v4.0.0.ipynb`
  - This is a Python notebook used to go through a directory of PDFs resulting from using the `extract_pdf_from_forum_dicom_v6.0.0.ipynb` script. Based on a csv provided by the user, it will filter the files and move only the selected PDFs to a directory of your choosing.

  - Upon opening the script in Jupyter, adjust the input and output directories to the desired locations in order to use the script (more detailed instructions are enclosed in the script). You will also need to adjust the subset file path so that it is the file path for the csv file that contains a subset of file names. 
  
  - **NOTE** The csv file being used to identify which PDF files to extract *must* contain a column called `"maskedID"` (case sensitive). The other columns it will utilize (if included in the csv file) are `"Date"` and `"Eye"` (also case sensitive).
  

