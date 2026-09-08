VERSION 5.00
Begin VB.Form Form 
   BackColor       =   &H00FFFFFF&
   Caption         =   "Form"
   ClientHeight    =   7440
   ClientLeft      =   120
   ClientTop       =   450
   ClientWidth     =   4755
   LinkTopic       =   "Form"
   ScaleHeight     =   7440
   ScaleWidth      =   4755
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer global 
      Interval        =   10
      Left            =   1320
      Top             =   6480
   End
   Begin VB.Timer temporizadorGlobal 
      Interval        =   10
      Left            =   840
      Top             =   6480
   End
   Begin VB.PictureBox figuraVerde 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   300
      Left            =   480
      Picture         =   "colision.frx":0000
      ScaleHeight     =   300
      ScaleWidth      =   300
      TabIndex        =   1
      Top             =   3360
      Width           =   300
   End
   Begin VB.PictureBox figuraPloma 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   300
      Left            =   2160
      Picture         =   "colision.frx":03B6
      ScaleHeight     =   300
      ScaleWidth      =   300
      TabIndex        =   0
      Top             =   480
      Width           =   300
   End
   Begin VB.Shape Shape4 
      FillStyle       =   7  'Diagonal Cross
      Height          =   495
      Left            =   0
      Top             =   0
      Width           =   4815
   End
   Begin VB.Shape Shape3 
      FillStyle       =   7  'Diagonal Cross
      Height          =   495
      Left            =   0
      Top             =   6960
      Width           =   4815
   End
   Begin VB.Shape Shape2 
      FillStyle       =   7  'Diagonal Cross
      Height          =   8175
      Left            =   4440
      Top             =   -720
      Width           =   375
   End
   Begin VB.Shape Shape1 
      FillStyle       =   7  'Diagonal Cross
      Height          =   8175
      Left            =   0
      Top             =   -720
      Width           =   375
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim velMov As Integer
Dim direccion As String


Private Sub figuraPloma_KeyDown(KeyCode As Integer, Shift As Integer)

    'Mover la figura ploma con las teclas de direccion
    If (KeyCode = vbKeyDown) Then
        figuraPloma.Top = figuraPloma.Top + velMov
    End If
    
    If (KeyCode = vbKeyUp) Then
        figuraPloma.Top = figuraPloma.Top - velMov
    End If
    
    If (KeyCode = vbKeyLeft) Then
        figuraPloma.Left = figuraPloma.Left - velMov
    End If
    
    If (KeyCode = vbKeyRight) Then
        figuraPloma.Left = figuraPloma.Left + velMov
    End If

End Sub

Private Sub Form_Load()

    velMov = 1000 'Velocidad de desplazamiento de la figura ploma
    direccion = "derecha" 'Direccion inicial de movimiento figura verde

End Sub

Private Sub detectarColision()

    If ((figuraPloma.Left < figuraVerde.Left + figuraVerde.Width) And (figuraVerde.Left < figuraPloma.Left + figuraPloma.Width) And (figuraPloma.Top < figuraVerde.Top + figuraVerde.Height) And (figuraVerde.Top < figuraPloma.Top + figuraPloma.Height)) Then
        'Colision entre objetos producida
        MsgBox ("Colisionaste con el objeto")
        figuraPloma.Top = 480
    End If
    
End Sub
Private Sub temporizadorGlobal_Timer()

    'Asegura que la figura ploma no se salga de los limites establecidos
    If (figuraPloma.Left <= 360) Then
        figuraPloma.Left = 360
    End If
    
    If (figuraPloma.Left >= 4200) Then
        figuraPloma.Left = 4200
    End If
    
    If (figuraPloma.Top <= 480) Then
        figuraPloma.Top = 480
    End If
    
    If (figuraPloma.Top >= 6720) Then
        figuraPloma.Top = 6720
    End If
    
    'Detectar colision
    detectarColision
    
    'Mover la figura verde de izquierda a derecha y viceversa
    If direccion = "derecha" Then
        figuraVerde.Left = figuraVerde.Left + 50
    ElseIf direccion = "izquierda" Then
        figuraVerde.Left = figuraVerde.Left - 50
    End If
    
    If figuraVerde.Left >= 4200 Then
        direccion = "izquierda"
    End If
    
    If figuraVerde.Left <= 360 Then
       direccion = "derecha"
    End If
      
End Sub
