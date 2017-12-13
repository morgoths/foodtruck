# foodtruck
webapp with R - Multivariate statistics for a foodtruck

![Alt text](foodtruck/exemple)

# With docker
- Build docker image

```
sudo docker build -t renzoscuderi/foodtruck .
```

- Running the image for testing 

```
sudo docker run -it -p 3838:3838 renzoscuderi/foodtruck
```

# With Rstudio
- Dowload folder "foodtruck"
- Install lib with install.packages (listed in app.r)
- Run app.r
