##equivalencia distribucional
a1=c(10,12,20)
a2=c(5,6,10)
a3=c(8,4,5)
perf.a1=a1/sum(a1)
perf.a2=a2/sum(a2)
perf.a3=a3/sum(a3)
totcol=c(23,22,35)
d13=sum((perf.a1-perf.a3)*totcol)
d23=sum((perf.a2-perf.a3)*totcol)
d12=sum((perf.a1-perf.a2)*totcol)
d12
d13
d23
  
AI=a1+a2
perf.AI=AI/sum(AI)
AII=a3
perf.AII=perf.a3
D12=sum((perf.AI-perf.AII)*totcol)

