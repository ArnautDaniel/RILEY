
%-------------------------------------------------------------------
\ProvidesClass{invoice}

\LoadClass[10.5pt]{article}

\usepackage[letterpaper,hmargin=0.79in,vmargin=0.79in]{geometry}
\usepackage[parfill]{parskip} % Do not indent paragraphs
\usepackage{fp} % Fixed-point arithmetic
\usepackage{calc} % Counters for totaling hours and cost
\usepackage{longtable}
\usepackage[table]{xcolor}

% Define color of grey lines
\definecolor{lightgray}{gray}{0.9}

% Keep plain for page numbers
\pagestyle{plain}
\linespread{1.5} % Line spacing

\setlength{\doublerulesep}{\arrayrulewidth} % Double rules look like one thick one

% Enable alternate grey and white lines on table
\let\oldlongtable\longtable
\let\endoldlongtable\endlongtable
\renewenvironment{longtable}{\rowcolors{2}{white}{lightgray}\oldlongtable} {
\endoldlongtable}

% Command for setting a default hourly rate
\newcommand{\feetype}[1]{
    \textbf{#1}
    \\
}

% Counters for totaling up hours and dollars
\newcounter{hours} \newcounter{subhours} \newcounter{cost} \newcounter{subcost}
\setcounter{hours}{0} \setcounter{subhours}{0} \setcounter{cost}{0} \setcounter{subcost}{0}
\newcounter{rowno}
\setcounter{rowno}{0}

% Formats inputed number with 2 digits after the decimal place
\newcommand*{\formatNumber}[1]{\FPround{\cost}{#1}{2}\cost} %


%------------------------------------
% Generate Table of Images
%------------------------------------
\newenvironment{pictureTable}{
	\newcommand*{\picrow}[3]{
		\includegraphics[height=4cm]{##1} & \includegraphics[height=4cm]{##2} &
		\includegraphics[height=4cm]{##3} \\
	}

	\begin{longtable}{|c|c|c|}
	{\bf Item Images} & {\bf Item Images} & {\bf Item Images} \\
	\hline \hline
	\endhead
}{ \hline
	\end{longtable}

}

%-----------------------------------
% Create an invoice table
%-----------------------------------
\newenvironment{invoiceTable}{
    % Create a new row from title, unit quantity, unit rate, and unit name
    \newcommand*{\unitrow}[4]{%
         \addtocounter{cost}{1000 * \real{##2} * \real{##3}}%
         \addtocounter{subcost}{1000 * \real{##2} * \real{##3}}%
        \stepcounter{rowno} \therowno & ##1 & ##2 & \$\formatNumber{##3} & & & ##4
         \\
    }

    \renewcommand{\tabcolsep}{1.0ex}
    \setlength\LTleft{0pt}
    \setlength\LTright{0pt}

    \begin{longtable}{| p{.75cm}|p{6.5cm}|c|c|c|p{2.5cm}|c|}
    \hline
    {\bf No.} & {\bf Item Description}  & {\bf QTY} & {\bf 1\textsuperscript{st} Wk Price}  & {\bf Ext. Price} & {\bf Notes} & {\bf RTN} \\*
    \hline\hline
    \endhead
}{
    \hline
    
    \end{longtable}
}
