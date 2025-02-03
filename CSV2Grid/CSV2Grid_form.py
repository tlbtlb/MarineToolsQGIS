# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'CSV2Grid_form.ui'
#
# Created by: PyQt5 UI code generator 5.15.4
#
# WARNING: Any manual changes made to this file will be lost when pyuic5 is
# run again.  Do not edit this file unless you know what you are doing.


from PyQt5 import QtCore, QtGui, QtWidgets


class Ui_CSV2Grid_form(object):
    def setupUi(self, CSV2Grid_form):
        CSV2Grid_form.setObjectName("CSV2Grid_form")
        CSV2Grid_form.setWindowModality(QtCore.Qt.ApplicationModal)
        CSV2Grid_form.setEnabled(True)
        CSV2Grid_form.resize(601, 626)
        font = QtGui.QFont()
        font.setBold(False)
        CSV2Grid_form.setFont(font)
        CSV2Grid_form.setMouseTracking(False)
        self.label_2 = QtWidgets.QLabel(CSV2Grid_form)
        self.label_2.setGeometry(QtCore.QRect(11, 11, 231, 16))
        font = QtGui.QFont()
        font.setFamily("Arial")
        font.setPointSize(10)
        font.setBold(False)
        self.label_2.setFont(font)
        self.label_2.setAlignment(QtCore.Qt.AlignBottom|QtCore.Qt.AlignLeading|QtCore.Qt.AlignLeft)
        self.label_2.setObjectName("label_2")
        self.lblCSV = QtWidgets.QLabel(CSV2Grid_form)
        self.lblCSV.setGeometry(QtCore.QRect(11, 67, 261, 16))
        font = QtGui.QFont()
        font.setBold(True)
        self.lblCSV.setFont(font)
        self.lblCSV.setText("")
        self.lblCSV.setAlignment(QtCore.Qt.AlignBottom|QtCore.Qt.AlignLeading|QtCore.Qt.AlignLeft)
        self.lblCSV.setObjectName("lblCSV")
        self.lsCSV = QtWidgets.QListWidget(CSV2Grid_form)
        self.lsCSV.setGeometry(QtCore.QRect(11, 120, 536, 102))
        self.lsCSV.setObjectName("lsCSV")
        self.LblStatus = QtWidgets.QLabel(CSV2Grid_form)
        self.LblStatus.setGeometry(QtCore.QRect(11, 362, 181, 16))
        font = QtGui.QFont()
        font.setBold(True)
        self.LblStatus.setFont(font)
        self.LblStatus.setText("")
        self.LblStatus.setObjectName("LblStatus")
        self.status = QtWidgets.QProgressBar(CSV2Grid_form)
        self.status.setGeometry(QtCore.QRect(11, 385, 531, 24))
        self.status.setProperty("value", 24)
        self.status.setObjectName("status")
        self.label = QtWidgets.QLabel(CSV2Grid_form)
        self.label.setGeometry(QtCore.QRect(11, 416, 231, 16))
        self.label.setObjectName("label")
        self.txtError = QtWidgets.QTextEdit(CSV2Grid_form)
        self.txtError.setGeometry(QtCore.QRect(11, 439, 536, 102))
        self.txtError.setObjectName("txtError")
        self.BtnApplyClose = QtWidgets.QDialogButtonBox(CSV2Grid_form)
        self.BtnApplyClose.setGeometry(QtCore.QRect(246, 560, 241, 24))
        self.BtnApplyClose.setOrientation(QtCore.Qt.Horizontal)
        self.BtnApplyClose.setStandardButtons(QtWidgets.QDialogButtonBox.Cancel|QtWidgets.QDialogButtonBox.Ok)
        self.BtnApplyClose.setCenterButtons(False)
        self.BtnApplyClose.setObjectName("BtnApplyClose")
        self.helpButton_2 = QtWidgets.QPushButton(CSV2Grid_form)
        self.helpButton_2.setGeometry(QtCore.QRect(10, 560, 93, 29))
        font = QtGui.QFont()
        font.setFamily("Arial")
        font.setPointSize(10)
        font.setBold(False)
        self.helpButton_2.setFont(font)
        self.helpButton_2.setObjectName("helpButton_2")
        self.layoutWidget = QtWidgets.QWidget(CSV2Grid_form)
        self.layoutWidget.setGeometry(QtCore.QRect(11, 34, 571, 31))
        self.layoutWidget.setObjectName("layoutWidget")
        self.gridLayout_3 = QtWidgets.QGridLayout(self.layoutWidget)
        self.gridLayout_3.setContentsMargins(0, 0, 0, 0)
        self.gridLayout_3.setObjectName("gridLayout_3")
        self.BtnInputFolder = QtWidgets.QPushButton(self.layoutWidget)
        self.BtnInputFolder.setEnabled(True)
        font = QtGui.QFont()
        font.setBold(False)
        self.BtnInputFolder.setFont(font)
        self.BtnInputFolder.setObjectName("BtnInputFolder")
        self.gridLayout_3.addWidget(self.BtnInputFolder, 0, 1, 1, 1)
        self.LinInputFolder = QtWidgets.QLineEdit(self.layoutWidget)
        self.LinInputFolder.setEnabled(True)
        self.LinInputFolder.setMouseTracking(True)
        self.LinInputFolder.setFocusPolicy(QtCore.Qt.StrongFocus)
        self.LinInputFolder.setAcceptDrops(False)
        self.LinInputFolder.setText("")
        self.LinInputFolder.setReadOnly(True)
        self.LinInputFolder.setObjectName("LinInputFolder")
        self.gridLayout_3.addWidget(self.LinInputFolder, 0, 0, 1, 1)
        self.layoutWidget1 = QtWidgets.QWidget(CSV2Grid_form)
        self.layoutWidget1.setGeometry(QtCore.QRect(11, 229, 531, 55))
        self.layoutWidget1.setObjectName("layoutWidget1")
        self.gridLayout_2 = QtWidgets.QGridLayout(self.layoutWidget1)
        self.gridLayout_2.setContentsMargins(0, 0, 0, 0)
        self.gridLayout_2.setObjectName("gridLayout_2")
        self.latitude_field = QtWidgets.QComboBox(self.layoutWidget1)
        self.latitude_field.setObjectName("latitude_field")
        self.gridLayout_2.addWidget(self.latitude_field, 1, 0, 1, 1)
        self.label_8 = QtWidgets.QLabel(self.layoutWidget1)
        self.label_8.setObjectName("label_8")
        self.gridLayout_2.addWidget(self.label_8, 0, 0, 1, 1)
        self.longitude_field = QtWidgets.QComboBox(self.layoutWidget1)
        self.longitude_field.setObjectName("longitude_field")
        self.gridLayout_2.addWidget(self.longitude_field, 1, 1, 1, 1)
        self.label_7 = QtWidgets.QLabel(self.layoutWidget1)
        self.label_7.setObjectName("label_7")
        self.gridLayout_2.addWidget(self.label_7, 0, 1, 1, 1)
        self.DepthZ = QtWidgets.QComboBox(self.layoutWidget1)
        self.DepthZ.setObjectName("DepthZ")
        self.gridLayout_2.addWidget(self.DepthZ, 1, 2, 1, 1)
        self.label_6 = QtWidgets.QLabel(self.layoutWidget1)
        self.label_6.setObjectName("label_6")
        self.gridLayout_2.addWidget(self.label_6, 0, 2, 1, 1)
        self.layoutWidget_2 = QtWidgets.QWidget(CSV2Grid_form)
        self.layoutWidget_2.setGeometry(QtCore.QRect(10, 290, 358, 31))
        self.layoutWidget_2.setObjectName("layoutWidget_2")
        self.horizontalLayout = QtWidgets.QHBoxLayout(self.layoutWidget_2)
        self.horizontalLayout.setContentsMargins(0, 0, 0, 0)
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.pushButton = QtWidgets.QPushButton(self.layoutWidget_2)
        self.pushButton.setMaximumSize(QtCore.QSize(150, 16777215))
        self.pushButton.setObjectName("pushButton")
        self.horizontalLayout.addWidget(self.pushButton)
        self.labelselectedcrs = QtWidgets.QTextBrowser(self.layoutWidget_2)
        self.labelselectedcrs.setMinimumSize(QtCore.QSize(0, 24))
        self.labelselectedcrs.setMaximumSize(QtCore.QSize(16777215, 24))
        self.labelselectedcrs.setBaseSize(QtCore.QSize(0, 24))
        self.labelselectedcrs.setVerticalScrollBarPolicy(QtCore.Qt.ScrollBarAlwaysOff)
        self.labelselectedcrs.setHorizontalScrollBarPolicy(QtCore.Qt.ScrollBarAlwaysOff)
        self.labelselectedcrs.setObjectName("labelselectedcrs")
        self.horizontalLayout.addWidget(self.labelselectedcrs)
        self.layoutWidget_3 = QtWidgets.QWidget(CSV2Grid_form)
        self.layoutWidget_3.setGeometry(QtCore.QRect(10, 320, 358, 31))
        self.layoutWidget_3.setObjectName("layoutWidget_3")
        self.horizontalLayout_2 = QtWidgets.QHBoxLayout(self.layoutWidget_3)
        self.horizontalLayout_2.setContentsMargins(0, 0, 0, 0)
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.pushButton_2 = QtWidgets.QPushButton(self.layoutWidget_3)
        self.pushButton_2.setMaximumSize(QtCore.QSize(150, 16777215))
        self.pushButton_2.setObjectName("pushButton_2")
        self.horizontalLayout_2.addWidget(self.pushButton_2)
        self.labelselectedcrs_2 = QtWidgets.QTextBrowser(self.layoutWidget_3)
        self.labelselectedcrs_2.setMinimumSize(QtCore.QSize(0, 24))
        self.labelselectedcrs_2.setMaximumSize(QtCore.QSize(16777215, 24))
        self.labelselectedcrs_2.setBaseSize(QtCore.QSize(0, 24))
        self.labelselectedcrs_2.setVerticalScrollBarPolicy(QtCore.Qt.ScrollBarAlwaysOff)
        self.labelselectedcrs_2.setHorizontalScrollBarPolicy(QtCore.Qt.ScrollBarAlwaysOff)
        self.labelselectedcrs_2.setObjectName("labelselectedcrs_2")
        self.horizontalLayout_2.addWidget(self.labelselectedcrs_2)
        self.label_3 = QtWidgets.QLabel(CSV2Grid_form)
        self.label_3.setGeometry(QtCore.QRect(380, 300, 151, 16))
        self.label_3.setObjectName("label_3")
        self.label_9 = QtWidgets.QLabel(CSV2Grid_form)
        self.label_9.setGeometry(QtCore.QRect(410, 320, 101, 20))
        self.label_9.setObjectName("label_9")
        self.label_5 = QtWidgets.QLabel(CSV2Grid_form)
        self.label_5.setGeometry(QtCore.QRect(520, 550, 61, 61))
        self.label_5.setText("")
        
        import os
        plugin_dir = os.path.dirname(__file__)
        NOCicon = plugin_dir + "/../icons/NOC.png"
        print(NOCicon)

        self.label_5.setPixmap(QtGui.QPixmap(NOCicon))
        self.label_5.setObjectName("label_5")
        self.Do_all = QtWidgets.QCheckBox(CSV2Grid_form)
        self.Do_all.setGeometry(QtCore.QRect(10, 80, 211, 18))
        font = QtGui.QFont()
        font.setPointSize(10)
        font.setBold(False)
        self.Do_all.setFont(font)
        self.Do_all.setChecked(False)
        self.Do_all.setObjectName("Do_all")
        self.Do_Grids = QtWidgets.QCheckBox(CSV2Grid_form)
        self.Do_Grids.setGeometry(QtCore.QRect(250, 80, 341, 18))
        font = QtGui.QFont()
        font.setPointSize(10)
        font.setBold(False)
        self.Do_Grids.setFont(font)
        self.Do_Grids.setCheckable(True)
        self.Do_Grids.setChecked(True)
        self.Do_Grids.setObjectName("Do_Grids")

        self.retranslateUi(CSV2Grid_form)
        self.BtnApplyClose.accepted.connect(CSV2Grid_form.accept)
        self.BtnApplyClose.rejected.connect(CSV2Grid_form.reject)
        QtCore.QMetaObject.connectSlotsByName(CSV2Grid_form)

    def retranslateUi(self, CSV2Grid_form):
        _translate = QtCore.QCoreApplication.translate
        CSV2Grid_form.setWindowTitle(_translate("CSV2Grid_form", "Batch Convert CSV to Grids"))
        self.label_2.setText(_translate("CSV2Grid_form", "Input CSV Point Folder"))
        self.label.setText(_translate("CSV2Grid_form", "Log"))
        self.helpButton_2.setText(_translate("CSV2Grid_form", "Help"))
        self.BtnInputFolder.setText(_translate("CSV2Grid_form", "Browse..."))
        self.label_8.setText(_translate("CSV2Grid_form", "Latitude (Y) Field"))
        self.label_7.setText(_translate("CSV2Grid_form", "Longitude (X) Field"))
        self.label_6.setText(_translate("CSV2Grid_form", "Attribute (Z) Field"))
        self.pushButton.setText(_translate("CSV2Grid_form", "Select Input CRS...   "))
        self.labelselectedcrs.setHtml(_translate("CSV2Grid_form", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><meta charset=\"utf-8\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Segoe UI\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'MS Shell Dlg 2\'; font-size:8pt;\">No CRS selected</span></p></body></html>"))
        self.pushButton_2.setText(_translate("CSV2Grid_form", "Select Output CRS..."))
        self.labelselectedcrs_2.setHtml(_translate("CSV2Grid_form", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><meta charset=\"utf-8\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Segoe UI\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:0px; margin-bottom:0px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-family:\'MS Shell Dlg 2\'; font-size:8pt;\">No CRS selected</span></p></body></html>"))
        self.label_3.setText(_translate("CSV2Grid_form", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><meta charset=\"utf-8\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Segoe UI\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Co-ordinate Reference</p></body></html>"))
        self.label_9.setText(_translate("CSV2Grid_form", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><meta charset=\"utf-8\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:\'Segoe UI\'; font-size:9pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:12px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\">Systems</p></body></html>"))
        self.Do_all.setText(_translate("CSV2Grid_form", "Convert all files? or select one"))
        self.Do_Grids.setText(_translate("CSV2Grid_form", "Convert to Grids? (not points)"))
