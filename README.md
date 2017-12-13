# foodtruck
webapp with R - Multivariate statistics for a foodtruck
# With docker
build docker image

```
sudo docker build -t renzoscuderi/foodtruck .
```

Running the image for testing 

```
sudo docker run -it -p 3838:3838 renzoscuderi/foodtruck
```

# With Rstudio
- Dowload folder "foodtruck"
install lib with install.packages (listed in app.r)
- run app.r
