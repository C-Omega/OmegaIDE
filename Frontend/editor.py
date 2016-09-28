#Editor
from PyQt4 import QtGui, Qsci

class Editor(Qsci.QsciScintilla):

    def __init__(self):
        Qsci.QsciScintilla.__init__(self)
        self.set_font()
        self.set_margin_line_numbers()

        lexer = Qsci.QsciLexerPython(self)
        self.setLexer(lexer)
    
    def set_font(self):
        self.font = QtGui.QFont()
        self.font.setFamily("Consolas")#TODO: Add preferences etc

        self.font.setPointSize(6)
        self.setFont(self.font)

    def set_margin_line_numbers(self):

        number_font = self.font
        number_font.setPointSize(8)
        fontmetrics = QtGui.QFontMetrics(number_font)
        self.setMarginsFont(number_font)
        self.setMarginWidth(0, 10)
        self.setMarginLineNumbers(0, True)
        self.setMarginsBackgroundColor(QtGui.QColor("#f0f0f0"))

    def get_plain_text(self):
        return self.text()