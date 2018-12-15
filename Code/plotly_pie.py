import plotly
import plotly.plotly as py
import plotly.graph_objs as go
import pandas as pd

plotly.tools.set_credentials_file(username='zz186', api_key='g7hnRhD8XruvpT3eKj1C')

data=pd.read_csv("FIFA19 - Ultimate Team players.csv")
df2=data[["quality","league"]]
df2["counts"]=1

df2=df2.groupby(['quality']).sum()
df2=df2.reset_index()
a=[]
b=[]
for i in range(len(df2)):
    a.append(df2.quality[i])
    b.append(df2.counts[i])

trace = go.Pie(labels=a, values=b,
               hoverinfo='label+value', textinfo='percent+label', 
               textfont=dict(size=20),
               marker=dict(
                           line=dict(color='#000000', width=2)))
data=[trace]

layout= {
        "title":"Different quality of players percentage",
        "annotations": [
            {
                "font": {
                    "size": 30
                },
                "showarrow": False,
                "text": "GHG",
                "x": 0.20,
                "y": 0.5
            }]
        }
                
fig = go.Figure(data=data, layout=layout)

py.plot(fig, filename='styled_pie_chart')

#py.iplot([trace], filename='styled_pie_chart')