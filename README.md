# Navigation

The scraped data is in the 'Cleaning.R' file. This process takes a little bit of time to scrape all the data. We saved two data sets in that script so we could skip this process when loading in the data.

'Model_Exploration.R' contains code on an example analysis of a the strike zone by count. It also contains logestic regression, ridge regression, and LASSO modeling.

'Trees.R' contains all modeling code on random forest and boosting. We save results periodically throughout the code to make it easy to use in the slides and paper.

The slides RMarkdown code is titled 'Slides.Rmd' with the pdf as 'Slides.pdf'

The paper RMarkdown code is titled 'Paper.Rmd' with the pdf as 'Paper.pdf'


## Description

Quantifying the best framing catcher's during the 2021 MLB season.

"Catcher framing is the art of a catcher receiving a pitch in a way that makes it more likely for an umpire to call it a strike -- whether that's turning a borderline ball into a strike, or not losing a strike to a ball due to poor framing." - [MLB.com Glossary](https://www.mlb.com/glossary/statcast/catcher-framing#:~:text=Catcher%20framing%20is%20the%20art,ball%20due%20to%20poor%20framing.) 

## Example
Below are two video examples of what's going on. 

The first video is a called strike for the Philadelphia Phillies catcher, J.T. Realmuto. What to look for is where the pitch is located relative to the little rectangle box, which is the electronic strike zone an umpire is supposed to make his calls off of. As you can see, the pitch is not inside of the box and theoretically should be a ball. However, it was called a strike. If you watch closely, what is noticable is how the catcher, Realmuto, presents the pitch to the umpire. He is able to convince the umpire this pitch is a strike even though it's not suppsed to be. Realmuto should be rewarded as a catcher for doing a good job.

https://user-images.githubusercontent.com/59383052/143179083-b97088f1-34bb-4788-9867-d9d66365e116.mp4

The opposite can also occur. In this video, Texas Rangers catcher, Jonah Heim doesn't get a called strike for a pitch well within the rectangle box. This could be because, if you watch closely, Heim moves his glove very suddenly as he catches the pitch. That could trick the eyes of the umpire, making him call this pitch a ball when it's clearly not a ball relative to the strike zone box. Heim should be punished for how he presented this pitch.

https://user-images.githubusercontent.com/59383052/143179143-7634fa08-5277-46ec-b5bc-fd8d577d098d.mp4

What this projects seeks to quantify is which catcher's are better than others at presenting pitches to umpires. It might be hard to justify each catcher with only one pitch, but many catchers recieve several hundreds, or thousands, of pitches each year so we will be able to tell which catchers are best at recieving pitches in a way to make it more likely for an umpire to call it a strike.



