CURRENT_DATE = $(shell date +"%Y%m%d")

clean:
	rm -rf ./elpa
	rm -rf ./.cask

install: clean
	cask install
