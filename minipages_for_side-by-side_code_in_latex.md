# Minipages for Side-by-side Code in LaTeX

This following code:

```latex
\begin{minipage}{0.5\textwidth}
\begin{lstlisting}{language=tex]
$$
\begin{aligned}
f\left( a \right) = \frac{1}{2\pi i}\oint_\gamma  \frac{f\left( z \right)}{z- a} \mathrm{d}z 
\end{aligned}
$$
\end{lstlisting}
\end{minipage}
\begin{minipage}{0.5\textwidth}
$$
\begin{aligned}
f\left( a \right) = \frac{1}{2\pi i}\oint_\gamma  \frac{f\left( z \right)}{z- a} \mathrm{d}z 
\end{aligned}
$$
\end{minipage}
```

produces the following output:

<!--- ![](/home/ryan/Dropbox/Notes/MD/_media/using-minipages-with-latex.png) --->

![](./../../../Notes/MD/_media/using-minipages-with-latex.png)
