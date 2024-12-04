# -*- coding: utf-8 -*-

################################################################################
## Form generated from reading UI file 'daintyafTunN.ui'
##
## Created by: Qt User Interface Compiler version 6.6.2
##
## WARNING! All changes made in this file will be lost when recompiling UI file!
################################################################################

from PySide6.QtCore import (QCoreApplication, QDate, QDateTime, QLocale,
    QMetaObject, QObject, QPoint, QRect,
    QSize, QTime, QUrl, Qt)
from PySide6.QtGui import (QBrush, QColor, QConicalGradient, QCursor,
    QFont, QFontDatabase, QGradient, QIcon,
    QImage, QKeySequence, QLinearGradient, QPainter,
    QPalette, QPixmap, QRadialGradient, QTransform)
from PySide6.QtWidgets import (QApplication, QDial, QLCDNumber, QMainWindow,
    QProgressBar, QSizePolicy, QStatusBar, QToolBar,
    QVBoxLayout, QWidget)

class Ui_Listcopy(object):
    def setupUi(self, Listcopy):
        if not Listcopy.objectName():
            Listcopy.setObjectName(u"Listcopy")
        Listcopy.resize(800, 600)
        self.centralwidget = QWidget(Listcopy)
        self.centralwidget.setObjectName(u"centralwidget")
        self.CopiedBar = QProgressBar(self.centralwidget)
        self.CopiedBar.setObjectName(u"CopiedBar")
        self.CopiedBar.setGeometry(QRect(130, 230, 32, 95))
        self.CopiedBar.setValue(24)
        self.CopiedBar.setOrientation(Qt.Vertical)
        self.ToCopyBar = QProgressBar(self.centralwidget)
        self.ToCopyBar.setObjectName(u"ToCopyBar")
        self.ToCopyBar.setGeometry(QRect(50, 140, 32, 95))
        self.ToCopyBar.setValue(24)
        self.ToCopyBar.setOrientation(Qt.Vertical)
        self.speedBar = QProgressBar(self.centralwidget)
        self.speedBar.setObjectName(u"speedBar")
        self.speedBar.setGeometry(QRect(209, 40, 32, 95))
        palette = QPalette()
        brush = QBrush(QColor(224, 27, 36, 255))
        brush.setStyle(Qt.SolidPattern)
        palette.setBrush(QPalette.Active, QPalette.Highlight, brush)
        brush1 = QBrush(QColor(230, 97, 0, 255))
        brush1.setStyle(Qt.SolidPattern)
        palette.setBrush(QPalette.Active, QPalette.Accent, brush1)
        self.speedBar.setPalette(palette)
        self.speedBar.setValue(24)
        self.speedBar.setOrientation(Qt.Vertical)
        self.speeddail = QDial(self.centralwidget)
        self.speeddail.setObjectName(u"speeddail")
        self.speeddail.setGeometry(QRect(310, 60, 201, 171))
        self.speeddail.setAutoFillBackground(True)
        self.speeddail.setStyleSheet(u"color: qlineargradient(spread:pad, x1:0, y1:0, x2:1, y2:0, stop:0 rgba(9, 41, 4, 255), stop:0.085 rgba(2, 79, 0, 255), stop:0.19 rgba(50, 147, 22, 255), stop:0.275 rgba(236, 191, 49, 255), stop:0.39 rgba(243, 61, 34, 255), stop:0.555 rgba(135, 81, 60, 255), stop:0.667 rgba(121, 75, 255, 255), stop:0.825 rgba(164, 255, 244, 255), stop:0.885 rgba(104, 222, 71, 255), stop:1 rgba(93, 128, 0, 255));")
        self.speeddail.setInvertedAppearance(True)
        self.speeddail.setNotchesVisible(True)
        self.widget = QWidget(self.centralwidget)
        self.widget.setObjectName(u"widget")
        self.widget.setGeometry(QRect(620, 60, 66, 54))
        self.verticalLayout = QVBoxLayout(self.widget)
        self.verticalLayout.setObjectName(u"verticalLayout")
        self.verticalLayout.setContentsMargins(0, 0, 0, 0)
        self.LedFilesCopied = QLCDNumber(self.widget)
        self.LedFilesCopied.setObjectName(u"LedFilesCopied")

        self.verticalLayout.addWidget(self.LedFilesCopied)

        self.LedFilesToCopy = QLCDNumber(self.widget)
        self.LedFilesToCopy.setObjectName(u"LedFilesToCopy")

        self.verticalLayout.addWidget(self.LedFilesToCopy)

        Listcopy.setCentralWidget(self.centralwidget)
        self.statusbar = QStatusBar(Listcopy)
        self.statusbar.setObjectName(u"statusbar")
        Listcopy.setStatusBar(self.statusbar)
        self.toolBar = QToolBar(Listcopy)
        self.toolBar.setObjectName(u"toolBar")
        Listcopy.addToolBar(Qt.TopToolBarArea, self.toolBar)

        self.retranslateUi(Listcopy)

        QMetaObject.connectSlotsByName(Listcopy)
    # setupUi

    def retranslateUi(self, Listcopy):
        Listcopy.setWindowTitle(QCoreApplication.translate("Listcopy", u"MainWindow", None))
#if QT_CONFIG(tooltip)
        self.CopiedBar.setToolTip(QCoreApplication.translate("Listcopy", u"Part that's left to copy", None))
#endif // QT_CONFIG(tooltip)
#if QT_CONFIG(whatsthis)
        self.CopiedBar.setWhatsThis(QCoreApplication.translate("Listcopy", u"Part not copied yet", None))
#endif // QT_CONFIG(whatsthis)
#if QT_CONFIG(accessibility)
        self.CopiedBar.setAccessibleName(QCoreApplication.translate("Listcopy", u"CopyBar", None))
#endif // QT_CONFIG(accessibility)
#if QT_CONFIG(tooltip)
        self.ToCopyBar.setToolTip(QCoreApplication.translate("Listcopy", u"Part that's left to copy", None))
#endif // QT_CONFIG(tooltip)
#if QT_CONFIG(whatsthis)
        self.ToCopyBar.setWhatsThis(QCoreApplication.translate("Listcopy", u"Part not copied yet", None))
#endif // QT_CONFIG(whatsthis)
#if QT_CONFIG(accessibility)
        self.ToCopyBar.setAccessibleName(QCoreApplication.translate("Listcopy", u"CopyBar", None))
#endif // QT_CONFIG(accessibility)
#if QT_CONFIG(tooltip)
        self.speedBar.setToolTip(QCoreApplication.translate("Listcopy", u"Part that's left to copy", None))
#endif // QT_CONFIG(tooltip)
#if QT_CONFIG(whatsthis)
        self.speedBar.setWhatsThis(QCoreApplication.translate("Listcopy", u"Part not copied yet", None))
#endif // QT_CONFIG(whatsthis)
#if QT_CONFIG(accessibility)
        self.speedBar.setAccessibleName(QCoreApplication.translate("Listcopy", u"CopyBar", None))
#endif // QT_CONFIG(accessibility)
#if QT_CONFIG(accessibility)
        self.speeddail.setAccessibleName(QCoreApplication.translate("Listcopy", u"Speedmeter", None))
#endif // QT_CONFIG(accessibility)
        self.toolBar.setWindowTitle(QCoreApplication.translate("Listcopy", u"toolBar", None))
    # retranslateUi

